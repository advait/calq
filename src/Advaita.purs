module Advaita where

import Prelude hiding (Unit)
import Control.Monad.State (StateT, lift)
import Control.Monad.State as State
import Data.BigNumber (BigNumber)
import Data.Either (Either(..))
import Data.Group (ginverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Utils (bigNum, undefinedLog)

type Unit
  = String

data Binding a
  = CannonicalUnit
  | DerivedUnit a
  | Alias String

data ParsedExpr
  = Scalar BigNumber
  | Name String
  | Fn2 { name :: String, p1 :: ParsedExpr, p2 :: ParsedExpr }
  --   | Fn1 { name :: String, p1 :: ParsedExpr }
  | CreateCannonicalUnit String
  | BindDerivedUnit { name :: String, expr :: ParsedExpr }
  | BindAlias { name :: String, target :: String }

type EvalValue
  = { scalar :: BigNumber, units :: Exponentials Unit }

type InterpreterState
  = Map String (Binding EvalValue)

initState :: InterpreterState
initState =
  Map.fromFoldable
    [ Tuple "pi" (DerivedUnit { scalar: bigNum "3.14159265359", units: mempty })
    , Tuple "c" (DerivedUnit { scalar: bigNum "299792458", units: Exponentials.quotient [ "m" ] [ "s" ] })
    -- , Tuple "π" (UnitValue (bigNum "3.14159265359") mempty)
    -- , Tuple "c" (UnitValue (bigNum "299792458") (CompUnit { num: SortedArray.fromFoldable [ Distance Meters ], den: SortedArray.fromFoldable [ Time Seconds ] }))
    ]

lookupName :: String -> StateT InterpreterState (Either String) (Binding EvalValue)
lookupName name = do
  state <- State.get
  case Map.lookup name state of
    Nothing -> lift $ Left ("Undefined variable " <> (show name))
    Just b -> pure b

eval :: ParsedExpr -> StateT InterpreterState (Either String) EvalValue
eval (Scalar scalar) = pure { scalar, units: mempty }

eval (CreateCannonicalUnit name) = do
  _ <- State.modify (\state -> Map.insert name CannonicalUnit state)
  pure $ { scalar: bigNum "1", units: Exponentials.singleton name }

eval (BindDerivedUnit { name, expr }) = do
  value <- eval expr
  _ <- State.modify (\state -> Map.insert name (DerivedUnit value) state)
  pure value

eval (BindAlias { name, target }) = do
  value <- eval $ Name target
  _ <- State.modify (\state -> Map.insert name (Alias target) state)
  pure value

-- | When evaluating a name, we are content with the result in terms of. CannonicalUnits
-- | as well as DerivedUnits. See `reduce` for a full reduction to CannonicalUnits.
eval (Name name) = do
  value <- lookupName name
  case value of
    CannonicalUnit -> pure { scalar: bigNum "1", units: Exponentials.singleton name }
    DerivedUnit _ -> pure { scalar: bigNum "1", units: Exponentials.singleton name }
    -- Recursively dereference aliases until we get to a CannonicalUnit or DerivedUnit.
    Alias name' -> eval (Name name')

eval (Fn2 { name: "*", p1, p2 }) = do
  e1 <- eval p1
  e2 <- eval p2
  pure $ e1 `times` e2

eval (Fn2 { name: "/", p1, p2 }) = do
  { scalar: s1, units: u1 } <- eval p1
  { scalar: s2, units: u2 } <- eval p2
  pure { scalar: s1 / s2, units: u1 <> (ginverse u2) }

eval (Fn2 { name, p1, p2 }) = do
  lift $ Left $ "Unknown function: " <> show name

times :: EvalValue -> EvalValue -> EvalValue
times { scalar: s1, units: u1 } { scalar: s2, units: u2 } = { scalar: s1 * s2, units: u1 <> u2 }

-- | Reduces a value such that units are all CannonicalUnits.
reduce :: EvalValue -> StateT InterpreterState (Either String) EvalValue
reduce { scalar: startScalar, units: startUnits } =
  let
    foldFn :: EvalValue -> Tuple Unit Int -> StateT InterpreterState (Either String) EvalValue
    foldFn acc@{ scalar, units } (Tuple newUnit power) = do
      value <- lookupName newUnit
      case value of
        CannonicalUnit -> pure { scalar, units: units <> (Exponentials.singleton newUnit) }
        DerivedUnit value' -> do
          reducedValue <- reduce value'
          pure $ acc `times` reducedValue
        Alias name -> undefinedLog "Do we really need to implement Alias here?"
  in
    Exponentials.foldM foldFn { scalar: startScalar, units: mempty } startUnits
