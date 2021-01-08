module Advaita where

import Prelude hiding (Unit)
import Control.Monad.State (StateT, lift, runStateT)
import Control.Monad.State as State
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Group (ginverse)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Parser (bigNumParser, lexeme)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (choice, notFollowedBy, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (eof, satisfy, string)
import Utils (bigNum, undefinedLog)

data ParsedExpr
  = Scalar BigNumber
  | Name String
  | Fn1 { name :: String, p1 :: ParsedExpr }
  | Fn2 { name :: String, p1 :: ParsedExpr, p2 :: ParsedExpr }
  | CreateCannonicalUnit String
  | BindDerivedUnit { name :: String, expr :: ParsedExpr }
  | BindAlias { name :: String, target :: String }

exprParser :: Parser String ParsedExpr
exprParser = compoundExprParser
  where
  singleExprParser =
    choice $ try
      <$> [ parenParser
        , createCanonicalUnitParser
        , fn2Parser
        , fn1Parser
        , bindDerivedUnitParser
        , bindAliasParser
        , scalarParser
        , Name <$> nameParser
        ]

  parenParser :: Parser String ParsedExpr
  parenParser = do
    _ <- lexeme $ string "("
    expr <- lexeme $ exprParser
    _ <- lexeme $ string ")"
    pure expr

  createCanonicalUnitParser :: Parser String ParsedExpr
  createCanonicalUnitParser = do
    _ <- lexeme $ string "createCanonicalUnit("
    name <- nameParser
    _ <- lexeme $ string ")"
    pure $ CreateCannonicalUnit name

  fn2Parser :: Parser String ParsedExpr
  fn2Parser =
    try
      $ do
          name <- lexeme $ (nameParser <* string "(")
          p1 <- lexeme $ exprParser
          _ <- lexeme $ string ","
          p2 <- lexeme $ exprParser
          _ <- lexeme $ string ")"
          pure $ Fn2 { name, p1, p2 }

  fn1Parser :: Parser String ParsedExpr
  fn1Parser =
    try
      $ do
          name <- lexeme $ (nameParser <* string "(")
          p1 <- lexeme $ exprParser
          _ <- lexeme $ string ")"
          pure $ Fn1 { name, p1 }

  bindDerivedUnitParser :: Parser String ParsedExpr
  bindDerivedUnitParser = do
    name <- nameParser
    _ <- lexeme $ string "="
    expr <- exprParser
    pure $ BindDerivedUnit { name, expr }

  bindAliasParser :: Parser String ParsedExpr
  bindAliasParser = do
    name <- nameParser
    _ <- lexeme $ string "->"
    target <- nameParser
    pure $ BindAlias { name, target }

  scalarParser :: Parser String ParsedExpr
  scalarParser = do
    value <- bigNumParser
    pure $ Scalar value

  nameParser :: Parser String String
  nameParser =
    let
      reserved = notFollowedBy $ lexeme $ string "in"

      name =
        lexeme
          $ do
              head <- satisfy isAlpha
              tail <- Array.many $ satisfy isAlphaNum
              pure $ fromCharArray $ Array.cons head tail
    in
      name

  compoundExprParser =
    let
      buildFn2 :: String -> ParsedExpr -> ParsedExpr -> ParsedExpr
      buildFn2 name p1 p2 = Fn2 { name, p1, p2 }
    in
      ( buildExprParser
          [ [ Infix (buildFn2 <$> (lexeme $ string "*")) AssocLeft
            , Infix (buildFn2 <$> (lexeme $ string "/")) AssocLeft
            ]
          , [ Infix (buildFn2 <$> (lexeme $ string "+")) AssocLeft
            , Infix (buildFn2 <$> (lexeme $ string "-")) AssocLeft
            ]
          , [ Infix (buildFn2 <$> (lexeme $ string "in")) AssocLeft ]
          ]
          (lexeme singleExprParser)
      )

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

initState :: InterpreterState
initState =
  Map.fromFoldable
    [ Tuple "pi" (DerivedUnit { scalar: bigNum "3.14159265359", units: mempty })
    , Tuple "π" (Alias "pi")
    , Tuple "c" (DerivedUnit { scalar: bigNum "299792458", units: Exponentials.quotient [ "m" ] [ "s" ] })
    , Tuple "m" CannonicalUnit
    , Tuple "ft" (DerivedUnit { scalar: bigNum "0.3048", units: Exponentials.singleton "m" })
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
  pure $ singletonUnit name

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
    CannonicalUnit -> pure $ singletonUnit name
    DerivedUnit _ -> pure $ singletonUnit name
    -- Recursively dereference aliases until we get to a CannonicalUnit or DerivedUnit.
    Alias name' -> eval (Name name')

eval (Fn1 { name, p1 }) = do
  lift $ Left $ "Unknown function: " <> show name

eval (Fn2 { name: "*", p1, p2 }) = do
  e1 <- eval p1
  e2 <- eval p2
  pure $ e1 `times` e2

eval (Fn2 { name: "/", p1, p2 }) = do
  e1 <- eval p1
  e2 <- eval p2
  pure $ e1 `dividedBy` e2

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
dividedBy { scalar: s1, units: u1 } { scalar: s2, units: u2 } = { scalar: s1 / s2, units: u1 <> (ginverse u2) }

-- | Returns whether the values are within .001% of each other.
approxEqual :: EvalValue -> EvalValue -> Boolean
approxEqual { scalar: s1, units: u1 } { scalar: s2, units: u2 } = (u1 == u2) && (abs (s1 - s2) / s1) < (bigNum "1e-5")

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
