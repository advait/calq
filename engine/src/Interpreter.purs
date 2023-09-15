module Interpreter where

import Prelude
import Control.Monad.State (StateT, evalStateT, execStateT, lift)
import Control.Monad.State as State
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
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
import Decimal (canPow, toNumber, parseDecimalUnsafe)
import Decimal as Decimal
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Expression (Expr(..), Value, ConcreteUnit, singletonUnit)
import Parsing (runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (TokenType, removeWhitespaceAndComments)
import Tokenizer as Tokenizer
import Utils (undefinedLog)
import Utils as Utils

-- | Our Interpreter evaluated Exprs into Values.
-- | It's possible that evaluations fail (e.g. unknown variables, fractional exponents, bad
-- | unit casting, etc.). Failures are indicated by (Either String).
-- | The Interpreter keeps track of variable bindings with a (StateT InterpreterState).
type Interpreter a = StateT InterpreterState (Either String) a

type InterpreterState =
  { bindings :: Map String Binding
  , prefixes :: List (Tuple String Value)
  }

-- | Names map to Bindings which come in different forms.
data Binding
  -- | A RootUnit is a fundamental/irreducible unit like "m" or "g".
  = RootUnit
  -- | A UnitBinding is something that's defined in terms of other Bindings like "ft = 0.3048 m".
  -- | A UnitBinding dereferences to itself. It's possible to reduce bindings (either with
  -- | reduce(binding) or with the "in" keyword) after which the binding recursively resolves to
  -- | the ultimate RootUnit (e.g. yard -> ft -> m).
  | UnitBinding Value
  -- | A NamedAlias immediately dereferences to the label it points to (e.g. meter -> m).
  -- | This is so we print consistent representations of units (e.g. don't want 1 meter*m).
  | NamedAlias String
  -- | A variable is like a UnitBinding except it can't be casted to.
  | Variable Value

-- | Describe how we want to dereference the underlying value.
-- | TODO(advait): Can't we eliminate this with a simple (dereferenceName >>= reduce)?
data DereferenceForm
  = Unreduced
  | Reduced

-- | Dereferences the underlying name to a given value. Follows the rules of Bindings above.
-- | If the given name can't be found, we try all of the prefixes in the order that the prefixes
-- | were defined.
dereferenceName :: String -> DereferenceForm -> Interpreter Value
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
    Just (Variable variable) -> case form of
      Unreduced -> pure variable
      Reduced -> reduce variable
    Nothing -> searchPrefixes state.prefixes
      where
      -- | TODO(advait): Support prefix aliases so that "kilometer" evaluates to "km" instead of "kilom".
      searchPrefixes :: List (Tuple String Value) -> Interpreter Value
      searchPrefixes Nil = lift $ Left ("Undefined variable " <> (show name))

      searchPrefixes ((Tuple prefix prefixValue) : tail) = case String.stripPrefix (Pattern prefix) name of
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

-- | Creates a new binding for the given name and value.
createBinding :: String -> Binding -> Interpreter Unit
createBinding name value = do
  _ <- State.modify (\state -> state { bindings = Map.insert name value state.bindings })
  pure unit

-- | Evaluate an Expr into a Value.
eval :: Expr -> Interpreter Value
eval (Scalar scalar) = pure { scalar, units: mempty }

eval (BindRootUnit { name }) = do
  _ <- createBinding name $ RootUnit
  pure $ singletonUnit name

eval (BindUnit { name, expr }) = do
  value <- eval expr
  _ <- createBinding name $ UnitBinding value
  pure $ singletonUnit name

eval (BindAlias { name, target }) = do
  _ <- createBinding name $ NamedAlias target
  pure $ singletonUnit target

eval (BindVariable { name, expr }) = do
  value <- eval expr
  _ <- createBinding name $ Variable value
  pure value

eval (BindPrefix { name, expr }) = do
  value <- eval expr
  _ <- State.modify (\state -> state { prefixes = List.snoc state.prefixes (Tuple name value) })
  pure value

eval (Name name) = dereferenceName name Unreduced

eval (Fn1 { name: "reduce", p1 }) = eval p1 >>= reduce

eval (Fn1 { name: "negate", p1 }) = do
  { scalar, units } <- eval p1
  pure { scalar: negate scalar, units }

eval (Fn1 { name, p1: _ }) = do
  lift $ Left $ "Unknown function: " <> show name

-- | Asserts that the two expressions are the same.
eval (Fn2 { name: "assertEqual", p1, p2 }) = do
  value1 <- eval p1
  value2 <- eval p2
  if value1 `approxEqual` value2 then
    pure $ value1
  else
    lift $ Left (show value1 <> " ≠ " <> show value2)

eval expr@(Fn2 { name: "^", p1, p2 }) = do
  e1 <- eval p1
  { scalar, units } <- eval p2
  let
    pow = Decimal.floor scalar
  if units /= mempty then
    lift $ Left ("Cannot raise powers with units: " <> show expr)
  else if pow < scalar then
    lift $ Left ("Cannot raise non-integer powers: " <> show expr)
  else if not $ canPow pow then
    lift $ Left ("Cannot raise to powers of more than 100: " <> show expr)
  else
    pure $ power e1 (Int.floor $ toNumber pow)

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
  when (desiredScalar /= one) (lift $ Left $ "Cannot cast to a numeric unit")
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

eval (Fn2 { name, p1: _, p2: _ }) = do
  lift $ Left $ "Unknown function: " <> show name

times :: Value -> Value -> Value
times { scalar: s1, units: u1 } { scalar: s2, units: u2 } = { scalar: s1 * s2, units: u1 <> u2 }

dividedBy :: Value -> Value -> Value
dividedBy { scalar: s1, units: u1 } { scalar: s2, units: u2 } =
  { scalar: s1 / s2
  , units: u1 <> (Group.ginverse u2)
  }

power :: Value -> Int -> Value
power { scalar, units } n =
  { scalar: scalar `Decimal.powInt` n
  , units: Group.power units n
  }

-- | Returns whether the values are within .001% of each other.
approxEqual :: Value -> Value -> Boolean
approxEqual { scalar: s1, units: u1 } { scalar: s2, units: u2 }
  | u1 /= u2 = false
  | s1 == zero = s1 == s2
  | otherwise = (abs (s1 - s2) / s1) < (parseDecimalUnsafe "1e-5")

-- | Casts the given value to the given unit.
cast :: Value -> Exponentials ConcreteUnit -> Interpreter Value
cast value desiredUnits = do
  { scalar, units } <- reduce $ value `dividedBy` { scalar: one, units: desiredUnits }
  when (units /= mempty) (lift $ Left $ "Cannot convert from " <> show value <> " to " <> show desiredUnits)
  pure { scalar, units: desiredUnits }

-- | Reduces a value such that units are all CannonicalUnits.
reduce :: Value -> Interpreter Value
reduce { scalar: startScalar, units: startUnits } =
  let
    foldFn :: Value -> Tuple ConcreteUnit Int -> Interpreter Value
    foldFn acc (Tuple newUnit newPower) = do
      value <- dereferenceName newUnit Reduced
      pure $ acc `times` (value `power` newPower)
  in
    Exponentials.foldM foldFn { scalar: startScalar, units: mempty } startUnits

-- | Determines whether the units can convert between each other.
convertible :: ConcreteUnit -> ConcreteUnit -> Interpreter Boolean
convertible u1 u2 = do
  { scalar: _, units } <- reduce $ (singletonUnit u1) `dividedBy` (singletonUnit u2)
  pure $ units == mempty

-- | Merge convertible units together (e.g. 1 ft*m will be merged to 0.3048 m^2).
mergeConvertible :: Value -> Interpreter Value
mergeConvertible ev@{ scalar: _, units } =
  let
    pairs :: List (Tuple (Tuple ConcreteUnit Int) (Tuple ConcreteUnit Int))
    pairs = List.fromFoldable $ Utils.nonrepeatedCombinations $ Exponentials.toArray units

    -- | Iterate through pairs. If we find a convertible pair, merge them and restart process.
    rec :: List (Tuple (Tuple ConcreteUnit Int) (Tuple ConcreteUnit Int)) -> Interpreter Value
    rec Nil = pure ev

    rec ((Tuple (Tuple u1 p1) (Tuple u2 p2)) : tail) = do
      isConvertible <- convertible u1 u2
      if isConvertible then do
        let
          unitsToMerge = { scalar: one, units: Exponentials.power u1 p1 <> Exponentials.power u2 p2 }
        conversionFactor <- reduce $ unitsToMerge
        mergeConvertible $ ev `dividedBy` unitsToMerge `times` conversionFactor
      else
        rec tail
  in
    rec pairs

-- | Execute a definitions file, yielding the final InterpreterState.
execDefinitions :: InterpreterState -> String -> InterpreterState
execDefinitions startState input =
  let
    tokens = case Tokenizer.tokenize input of
      Left err -> undefinedLog $ "This happens when tokenization fails (should never fail): " <> show err
      Right t -> t

    lines :: Array (Array TokenType)
    lines = Array.filter (not <<< Array.null) $ removeWhitespaceAndComments <$> Tokenizer.lines tokens

    execLine :: Array TokenType -> Interpreter Value
    execLine line =
      let
        expr = case runParser line (tokenExprParser <* eof) of
          Left err -> undefinedLog $ "Failed to parse definitions: " <> show err
          Right e -> e
      in
        eval expr

    result :: StateT InterpreterState (Either String) Unit
    result = traverse_ execLine lines
  in
    case execStateT result startState of
      Left err -> undefinedLog $ "Failed to exec definitions: " <> show err
      Right state -> state

-- | Initial state after the Definitions.calq file is executed.
initState :: InterpreterState
initState = execDefinitions emptyState Utils.definitionsFile
  where
  emptyState =
    { bindings: Map.empty
    , prefixes: Nil
    }

-- | Runs the interpreter, providing the evaluated value.
runInterpreter :: forall a. Interpreter a -> Either String a
runInterpreter i = evalStateT i initState
