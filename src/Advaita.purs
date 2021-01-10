module Advaita where

import Prelude hiding (Unit)
import Control.Monad.State (StateT, lift, runStateT)
import Control.Monad.State as State
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.BigNumber as BigNumber
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Group as Group
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Expressions (ParsedExpr(..), exprParser)
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Text.Parsing.Parser.String (eof)
import Utils (bigNum, undefinedLog)
import Utils as Utils

type Unit
  = String

type EvalValue
  = { scalar :: BigNumber, units :: Exponentials Unit }

singletonUnit :: Unit -> EvalValue
singletonUnit unit = { scalar: bigNum "1", units: Exponentials.singleton unit }

data Binding a
  = CannonicalUnit
  | DerivedUnit a
  | Alias String

type InterpreterState
  = Map String (Binding EvalValue)

type Interpreter a
  = StateT InterpreterState (Either String) a

initState :: InterpreterState
initState =
  Map.fromFoldable
    [ Tuple "pi" (DerivedUnit { scalar: bigNum "3.14159265359", units: mempty })
    , Tuple "π" (Alias "pi")
    , Tuple "c" (DerivedUnit { scalar: bigNum "299792458", units: Exponentials.quotient [ "m" ] [ "s" ] })
    , Tuple "m" CannonicalUnit
    , Tuple "ft" (DerivedUnit { scalar: bigNum "0.3048", units: Exponentials.singleton "m" })
    ]

lookupName :: String -> Interpreter (Binding EvalValue)
lookupName name = do
  state <- State.get
  case Map.lookup name state of
    Nothing -> lift $ Left ("Undefined variable " <> (show name))
    Just b -> pure b

eval :: ParsedExpr -> Interpreter EvalValue
eval (Scalar scalar) = pure { scalar, units: mempty }

eval (CreateCannonicalUnit name) = do
  _ <- State.modify (\state -> Map.insert name CannonicalUnit state)
  pure $ singletonUnit name

eval (BindDerivedUnit { name, expr }) = do
  value <- eval expr
  _ <- State.modify (\state -> Map.insert name (DerivedUnit value) state)
  pure value

eval (BindAlias { name, target }) = do
  value <- eval $ Name target
  _ <- State.modify (\state -> Map.insert name (Alias target) state)
  pure value

-- | When evaluating a name, we are content with the result in terms of. CannonicalUnits as well as
-- | DerivedUnits. See `reduc` for a full reduction to CannonicalUnits.
eval (Name name) = do
  value <- lookupName name
  case value of
    CannonicalUnit -> pure $ singletonUnit name
    DerivedUnit _ -> pure $ singletonUnit name
    -- Recursively dereference aliases until we get to a CannonicalUnit or DerivedUnit.
    Alias name' -> eval (Name name')

eval (Fn1 { name: "reduce", p1 }) = eval p1 >>= reduce

eval (Fn1 { name, p1 }) = do
  lift $ Left $ "Unknown function: " <> show name

eval (Fn2 { name: "*", p1, p2 }) = do
  e1 <- eval p1
  e2 <- eval p2
  mergeConvertible $ e1 `times` e2

eval (Fn2 { name: "/", p1, p2 }) = do
  e1 <- eval p1
  e2 <- eval p2
  mergeConvertible $ e1 `dividedBy` e2

eval (Fn2 { name: "in", p1, p2 }) = do
  e1 <- eval p1 >>= reduce
  e2'@{ scalar: desiredScalar, units: desiredUnits } <- eval p2
  e2 <- reduce e2'
  when (desiredScalar /= bigNum "1") (lift $ Left $ "Cannot cast to a numeric unit")
  let
    { scalar, units } = e1 `dividedBy` e2
  when (units /= mempty) (lift $ Left $ "Cannot convert from TODO")
  pure { scalar, units: desiredUnits }

eval (Fn2 { name, p1, p2 }) = do
  lift $ Left $ "Unknown function: " <> show name

times :: EvalValue -> EvalValue -> EvalValue
times { scalar: s1, units: u1 } { scalar: s2, units: u2 } = { scalar: s1 * s2, units: u1 <> u2 }

dividedBy :: EvalValue -> EvalValue -> EvalValue
dividedBy { scalar: s1, units: u1 } { scalar: s2, units: u2 } =
  { scalar: s1 / s2
  , units: u1 <> (Group.ginverse u2)
  }

power :: EvalValue -> Int -> EvalValue
power { scalar, units } n =
  { scalar: scalar `BigNumber.pow` (bigNum $ show n)
  , units: Group.power units n
  }

-- | Returns whether the values are within .001% of each other.
approxEqual :: EvalValue -> EvalValue -> Boolean
approxEqual { scalar: s1, units: u1 } { scalar: s2, units: u2 } = (u1 == u2) && (abs (s1 - s2) / s1) < (bigNum "1e-5")

-- | Reduces a value such that units are all CannonicalUnits.
reduce :: EvalValue -> Interpreter EvalValue
reduce { scalar: startScalar, units: startUnits } =
  let
    foldFn :: EvalValue -> Tuple Unit Int -> Interpreter EvalValue
    foldFn acc@{ scalar, units } (Tuple newUnit newPower) = do
      value <- lookupName newUnit
      case value of
        -- For a CannonicalUnit, we are done - simply return the unit with the appropriate power
        CannonicalUnit -> pure { scalar, units: units <> (Exponentials.power newUnit newPower) }
        -- DerivedUnits must be reduced further
        DerivedUnit value' -> do
          reducedValue <- reduce $ value' `power` newPower
          pure $ acc `times` reducedValue
        -- We should not see aliases when we are reducing as they should already be evaluated
        Alias name -> undefinedLog "We should not see Aliases when reducing"
  in
    Exponentials.foldM foldFn { scalar: startScalar, units: mempty } startUnits

-- | Determines whether the units can convert between each other.
convertible :: Unit -> Unit -> Interpreter Boolean
convertible u1 u2 = do
  { scalar, units } <- reduce $ ((singletonUnit u1) `dividedBy` (singletonUnit u2))
  let
    ret = units == mempty
  pure $ units == mempty

-- | Merge convertible units together (e.g. 1 ft*m will be merged to 0.3048 m^2).
mergeConvertible :: EvalValue -> Interpreter EvalValue
mergeConvertible ev@{ scalar: _, units } =
  let
    pairs :: List (Tuple (Tuple Unit Int) (Tuple Unit Int))
    pairs = List.fromFoldable $ Utils.nonrepeatedCombinations $ Exponentials.toArray units

    -- | Iterate through pairs. If we find a convertible pair, merge them and restart process.
    rec :: List (Tuple (Tuple Unit Int) (Tuple Unit Int)) -> Interpreter EvalValue
    rec Nil = pure ev

    rec ((Tuple (Tuple u1 p1) (Tuple u2 p2)) : tail) = do
      isConvertible <- convertible u1 u2
      if isConvertible then do
        let
          unitsToMerge = { scalar: bigNum "1", units: Exponentials.power u1 p1 <> Exponentials.power u2 p2 }
        conversionFactor <- reduce $ unitsToMerge
        mergeConvertible $ ev `dividedBy` unitsToMerge `times` conversionFactor
      else
        rec tail
  in
    rec pairs

evalProgramAll :: String -> Array (Either String EvalValue)
evalProgramAll input = Array.fromFoldable $ rec (List.fromFoldable exprs) initState
  where
  exprs :: Array (Either String ParsedExpr)
  exprs =
    fmapL parseErrorMessage
      <$> (flip runParser $ exprParser <* eof)
      <$> split (Pattern "\n") input

  rec :: List (Either String ParsedExpr) -> InterpreterState -> List (Either String EvalValue)
  rec Nil _ = Nil

  rec ((Left err) : tail) state = (Left err) : (rec tail state)

  rec ((Right head) : tail) state = case runStateT (eval head) state of
    Left e -> (Left e) : (rec tail state)
    Right (Tuple v state') -> (Right v) : (rec tail state')
