module Interpreter where

import Prelude hiding (Unit)

import Control.Monad.State (StateT, execStateT, lift, runStateT)
import Control.Monad.State as State
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.BigNumber as BigNumber
import Data.Either (Either(..))
import Data.Group as Group
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Expression (Line, ParsedExpr(..), parseLines)
import Math as Math
import Parsing (bigNum)
import Utils as Utils

type Unit
  = String

-- | Our interpreter evaluates expressions into these values.
type EvalValue
  = { scalar :: BigNumber, units :: Exponentials Unit }

scalar1 :: EvalValue
scalar1 = { scalar : bigNum "1", units: mempty}

prettyBigNum :: BigNumber -> String
prettyBigNum n
  | n - (Utils.bigNumberFixed 0 n) < bigNum ".01" = Utils.bigNumberFormatFixed 0 n
  | n - (Utils.bigNumberFixed 1 n) < bigNum ".01" = Utils.bigNumberFormatFixed 1 n
  | otherwise = Utils.bigNumberFormatFixed 2 n

prettyEvalValue :: EvalValue -> String
prettyEvalValue { scalar, units }
  | units == mempty = prettyBigNum scalar
  | otherwise = prettyBigNum scalar <> " " <> show units

singletonUnit :: Unit -> EvalValue
singletonUnit unit = { scalar: bigNum "1", units: Exponentials.singleton unit }

data Binding a
  = RootUnit
  | UnitBinding a
  | NamedAlias String
  | Variable a

type InterpreterState
  = { bindings :: Map String (Binding EvalValue)
    , prefixes :: List (Tuple String EvalValue)
    }

type Interpreter a
  = StateT InterpreterState (Either String) a

initState :: InterpreterState
initState = execDefinitions Utils.definitionsFile

data DereferenceForm = Unreduced | Reduced

dereferenceName :: String -> DereferenceForm -> Interpreter EvalValue
dereferenceName name form = do
  state :: InterpreterState <- State.get
  case Map.lookup name state.bindings of
    Just RootUnit -> pure $ singletonUnit name
    Just (UnitBinding derived) -> case form of
      Unreduced -> pure $ singletonUnit name
      Reduced -> reduce derived
    Just (NamedAlias target) -> case form of
      Unreduced -> pure $ singletonUnit target
      Reduced -> dereferenceName target form
    Just (Variable value) -> case form of
      Unreduced -> pure value
      Reduced -> reduce value
    Nothing -> searchPrefixes state.prefixes
      where
        searchPrefixes :: List (Tuple String EvalValue) -> Interpreter EvalValue
        searchPrefixes Nil = lift $ Left ("Undefined variable " <> (show name))
        searchPrefixes ((Tuple prefix prefixValue):tail) =
          case String.stripPrefix (Pattern prefix) name of
            Nothing -> searchPrefixes tail
            Just suffix -> case Map.lookup suffix state.bindings of
              Nothing -> searchPrefixes tail
              Just RootUnit -> case form of
                Unreduced -> pure $ singletonUnit name
                Reduced -> pure $ prefixValue `times` (singletonUnit suffix)
              Just (UnitBinding derived) -> case form of
                Unreduced -> pure $ singletonUnit name
                Reduced -> reduce $ prefixValue `times` derived
              Just (NamedAlias target) -> dereferenceName (prefix <> target) form
              Just (Variable _) -> lift $ Left ("Undefined variable " <> (show name))


setName :: String -> Binding EvalValue -> Interpreter (Binding EvalValue)
setName name value = do
  _ <- State.modify (\state -> state { bindings = Map.insert name value state.bindings })
  pure value

-- | Evaluate an expression, potentially updating state and returning an EvalValue.
eval :: ParsedExpr -> Interpreter EvalValue
eval (Scalar scalar) = pure { scalar, units: mempty }

eval (BindRootUnit { name }) = do
  _ <- setName name $ RootUnit
  pure $ singletonUnit name

eval (BindUnit { name, expr }) = do
  value <- eval expr
  _ <- setName name $ UnitBinding value
  pure value

eval (BindAlias { name, target }) = do
  _ <- setName name $ NamedAlias target
  pure $ singletonUnit target

eval (BindVariable { name, expr }) = do
  value <- eval expr
  _ <- setName name $ Variable value
  pure value

eval (BindPrefix { name, expr}) = do
  value <- eval expr
  _ <- State.modify (\state -> state { prefixes = List.snoc state.prefixes (Tuple name value) } )
  pure value

eval (Name name) = dereferenceName name Unreduced

eval (Fn1 { name: "reduce", p1 }) = eval p1 >>= reduce

eval (Fn1 { name: "negate", p1 }) = do
  { scalar, units } <- eval p1
  pure { scalar: negate scalar, units }

eval (Fn1 { name, p1 }) = do
  lift $ Left $ "Unknown function: " <> show name

-- | Asserts that the two expressions are the same.
eval (Fn2 { name: "assertEqual", p1, p2 }) = do
  value1 <- eval p1
  value2 <- eval p2
  if value1 `approxEqual` value2 then
    pure $ value1
  else
    lift $ Left (show value1 <> " ≠ " <> show value2)

eval (Fn2 { name: "^", p1, p2 }) = do
  e1 <- eval p1
  e2@{ scalar, units } <- eval p2
  let
    scalar' = BigNumber.toNumber scalar

    pow = Math.floor $ BigNumber.toNumber scalar
  if units /= mempty then
    lift $ Left ("Cannot raise powers with units: " <> show e2)
  else if pow < scalar' then
    lift $ Left ("Cannot raise non-integer powers" <> show e2)
  else
    pure $ power e1 (Int.floor pow)

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
  { scalar: desiredScalar, units: desiredUnits } <- eval p2
  when (desiredScalar /= bigNum "1") (lift $ Left $ "Cannot cast to a numeric unit")
  cast e1 desiredUnits

eval (Fn2 { name: "+", p1, p2 }) = do
  e1@{ scalar: _, units: desiredUnits } <- eval p1
  { scalar: s1, units: u1 } <- reduce e1
  { scalar: s2, units: u2 } <- eval p2 >>= reduce
  if u1 /= u2 then
    lift $ Left $ ("Cannot add units " <> show u1 <> " and " <> show u2)
  else
    cast { scalar: s1 + s2, units: u1 } desiredUnits

eval (Fn2 { name: "-", p1, p2 }) = do
  eval $ Fn2 { name: "+", p1, p2: Fn1 { name: "negate", p1: p2 } }

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
approxEqual { scalar: s1, units: u1 } { scalar: s2, units: u2 }
  | u1 /= u2 = false
  | s1 == (bigNum "0") = s1 == s2
  | otherwise = (abs (s1 - s2) / s1) < (bigNum "1e-5")

-- | Casts the given value to the given unit.
cast :: EvalValue -> Exponentials Unit -> Interpreter EvalValue
cast value desiredUnits = do
  { scalar, units } <- reduce $ value `dividedBy` { scalar: bigNum "1", units: desiredUnits }
  when (units /= mempty) (lift $ Left $ "Cannot convert from " <> show value <> " to " <> show desiredUnits)
  pure { scalar, units: desiredUnits }

-- | Reduces a value such that units are all CannonicalUnits.
reduce :: EvalValue -> Interpreter EvalValue
reduce { scalar: startScalar, units: startUnits } =
  let
    foldFn :: EvalValue -> Tuple Unit Int -> Interpreter EvalValue
    foldFn acc@{ scalar, units } (Tuple newUnit newPower) = do
      value <- dereferenceName newUnit Reduced
      pure $ acc `times` (value `power` newPower)
  in
    Exponentials.foldM foldFn { scalar: startScalar, units: mempty } startUnits

-- | Determines whether the units can convert between each other.
convertible :: Unit -> Unit -> Interpreter Boolean
convertible u1 u2 = do
  { scalar: _, units } <- reduce $ (singletonUnit u1) `dividedBy` (singletonUnit u2)
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

-- | Executes a calq program returning the final state. Useful for Definitions.
execDefinitions :: String -> InterpreterState
execDefinitions input =
  let
    lines :: Array (Either String Line)
    lines = parseLines input

    finalResult = execLines $ List.fromFoldable lines

    execLines :: List (Either String Line) -> Interpreter String
    execLines Nil = pure ""

    execLines ((Left err) : tail) = Utils.undefinedLog $ "Error parsing Definitions.calq: " <> err

    -- TODO(advait): Find a better representation of Noops than `Left ""`.
    execLines ((Right Nothing) : tail) = execLines tail

    execLines ((Right (Just expr)) : tail) = do
      res <- eval expr
      execLines tail
  in
    case execStateT finalResult mempty of
      Left err -> Utils.undefinedLog $ "Error executing Definitions.calq: " <> err
      Right s -> s

-- | Executes a calq program returning a list of error or result values.
evalProgram :: String -> Array (Either String EvalValue)
evalProgram input = Array.fromFoldable $ rec (List.fromFoldable lines) initState
  where
  lines = parseLines input

  rec :: List (Either String Line) -> InterpreterState -> List (Either String EvalValue)
  rec Nil _ = Nil

  rec ((Left err) : tail) state = (Left err) : (rec tail state)

  -- TODO(advait): Find a better representation of Noops than `Left ""`.
  rec ((Right Nothing) : tail) state = (Left "") : (rec tail state)

  rec ((Right (Just head)) : tail) state = case runStateT (eval head) state of
    Left e -> (Left e) : (rec tail state)
    Right (Tuple v state') -> (Right v) : (rec tail state')

-- | Executes the program, returning strings corresponding to the evaluation of each line.
evalProgramShow :: String -> Array String
evalProgramShow input = showItem <$> evalProgram input
  where
  showItem (Left s) = s

  showItem (Right value) = prettyEvalValue value
