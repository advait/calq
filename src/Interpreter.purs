module Interpreter where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Trans as State
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, foldM, fromFoldable, last, many)
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.SortedArray as SortedArray
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Parser (bigNum, bigNumParser, compUnitParser, lexeme)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (choice, sepBy, try)
import Text.Parsing.Parser.String (eof, satisfy, string)
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), TimeUnit(..), convertCompUnit)

-- | A value is the final/reduced form of an expression.
type Value
  = Tuple BigNumber CompUnit

valueParser :: Parser String Value
valueParser = do
  num <- bigNumParser
  unit <- (try compUnitParser) <|> (pure mempty)
  pure $ Tuple num unit

-- | An `Expr` is an expression that can be evaluated into a value.
data Expr
  = Constant Value CompUnit
  | Bind String Expr
  | Ref String
  | AssertEqual Expr Expr

exprParser :: Parser String Expr
exprParser = choice $ try <$> [ assertEqualParser, constantParser, bindParser, refParser ]
  where
  constantParser :: Parser String Expr
  constantParser = do
    value@(Tuple _ unit) <- valueParser
    showUnit <- (try $ Just <$> ((lexeme $ string "in") *> compUnitParser)) <|> (pure Nothing)
    pure $ Constant value (fromMaybe unit showUnit)

  nameParser :: Parser String String
  nameParser =
    lexeme
      $ do
          head <- satisfy isAlpha
          tail <- many $ satisfy isAlphaNum
          pure $ fromCharArray $ cons head tail

  bindParser :: Parser String Expr
  bindParser = do
    name <- nameParser
    _ <- lexeme $ (string "=" <|> string ":=" <|> string ":")
    expr <- exprParser
    pure $ Bind name expr

  refParser :: Parser String Expr
  refParser = Ref <$> nameParser

  assertEqualParser :: Parser String Expr
  assertEqualParser = do
    _ <- lexeme $ string "assertEqual("
    e1 <- lexeme $ exprParser
    _ <- lexeme $ string ","
    e2 <- lexeme $ exprParser
    _ <- lexeme $ string ")"
    pure $ AssertEqual e1 e2

programParser :: Parser String (Array Expr)
programParser = (fromFoldable <$> (exprParser `sepBy` (lexeme $ string "\n"))) <* eof

type InterpreterState
  = Map String Value

initState :: InterpreterState
initState =
  Map.fromFoldable
    [ Tuple "pi" (Tuple (bigNum "3.14159265359") mempty)
    , Tuple "π" (Tuple (bigNum "3.14159265359") mempty)
    , Tuple "c" (Tuple (bigNum "299792458") (CompUnit { num: SortedArray.fromFoldable [ Distance Meters ], den: SortedArray.fromFoldable [ Time Seconds ] }))
    ]

-- | Evaluates an expression.
eval :: Expr -> StateT InterpreterState (Either String) Value
-- | A constant expression evaluates to itself.
eval (Constant (Tuple value fromUnit) toUnit) = do
  ratio <- lift $ convertCompUnit fromUnit toUnit
  pure (Tuple (value * ratio) toUnit)

-- | Binds a name to its evaluated value.
eval (Bind name expr) = do
  value <- eval expr
  _ <- State.modify (\state -> Map.insert name value state)
  pure value

-- | Dereferences a name to its previously evaluated value.
eval (Ref name) = do
  state :: InterpreterState <- State.get
  case Map.lookup name state of
    Nothing -> lift $ Left ("Undefined variable " <> (show name))
    Just value -> pure $ value

-- | Asserts that the two expressions are the same.
eval (AssertEqual e1 e2) = do
  value1@(Tuple v1 u1) <- eval e1
  value2@(Tuple v2 u2) <- eval e2
  if v1 == v2 && u1 == u2 then
    pure $ Tuple v1 u1
  else
    lift $ Left (show value1 <> " ≠ " <> show value2)

-- Evaluates an entire program.
evalProgram :: Array Expr -> Either String Value
evalProgram exprs = evalStateT stateT initState
  where
  stateT = foldM (const eval) (Tuple (bigNum "1.0") mempty) exprs

evalProgram' :: String -> Either String Value
evalProgram' input = do
  program :: Array Expr <-
    fmapL parseErrorMessage
      $ sequence
      $ ((flip runParser) exprParser)
      <$> split (Pattern "\n") input
  evalProgram program
