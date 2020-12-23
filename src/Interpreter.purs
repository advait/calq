module Interpreter where

import Prelude hiding (div)
import Control.Alt ((<|>))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Trans as State
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, foldM, fromFoldable, many)
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.SortedArray as SortedArray
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Parser (bigNumParser, compUnitParser, lexeme)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (choice, sepBy, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (eof, satisfy, string)
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), TimeUnit(..), convertCompUnit, div, simplify, times)
import Utils (bigNum)

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
  | Fn2 String Expr Expr

exprParser :: Parser String Expr
exprParser = compoundExprParser
  where
  singleExprParser =
    choice $ try
      <$> [ parenParser
        , fn2Parser
        , bindParser
        , constantParser
        , refParser
        ]

  parenParser :: Parser String Expr
  parenParser = do
    _ <- lexeme $ string "("
    expr <- lexeme $ exprParser
    _ <- lexeme $ string ")"
    pure expr

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

  fn2Parser :: Parser String Expr
  fn2Parser =
    try
      $ do
          fnName <- lexeme $ (nameParser <* string "(")
          e1 <- lexeme $ exprParser
          _ <- lexeme $ string ","
          e2 <- lexeme $ exprParser
          _ <- lexeme $ string ")"
          pure $ Fn2 fnName e1 e2

  compoundExprParser =
    ( buildExprParser
        [ [ Infix (Fn2 <$> (lexeme $ string "*")) AssocLeft
          , Infix (Fn2 <$> (lexeme $ string "/")) AssocLeft
          ]
        ]
        (lexeme singleExprParser)
    )

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
    Just value -> pure value

-- | Asserts that the two expressions are the same.
eval (Fn2 "assertEqual" e1 e2) = do
  value1 <- eval e1
  value2 <- eval e2
  if value1 `approxEqual` value2 then
    pure $ value1
  else
    lift $ Left (show value1 <> " ≠ " <> show value2)

-- | Multiplication
eval (Fn2 "*" e1 e2) = do
  (Tuple v1 u1) <- eval e1
  (Tuple v2 u2) <- eval e2
  pure $ simplify $ Tuple (v1 * v2) (u1 `times` u2)

-- | Division
eval (Fn2 "/" e1 e2) = do
  (Tuple v1 u1) <- eval e1
  (Tuple v2 u2) <- eval e2
  pure $ simplify $ Tuple (v1 / v2) (u1 `div` u2)

-- | Unknown functions
eval (Fn2 name _ _) = lift $ Left ("Unkown function name " <> (show name))

-- | Returns whether the values are within .001% of each other.
approxEqual :: Value -> Value -> Boolean
approxEqual (Tuple v1 u1) (Tuple v2 u2) = (u1 == u2) && (abs (v1 - v2) / v1) < (bigNum "1e-5")

-- | Evaluates an entire program.
evalProgram :: Array Expr -> Either String Value
evalProgram exprs = evalStateT stateT initState
  where
  stateT = foldM (const eval) (Tuple (bigNum "1.0") mempty) exprs

-- TODO(advait): This is function exists because `lexeme` consums newline whitspace.
-- as a reuslt we have to manually split the program by newlines, parse each line,
-- and execute it. We need to rewrite lexeme and the corresponding integer/float parsers
-- to avoid chomping newlines.
evalProgram' :: String -> Either String Value
evalProgram' input = do
  program :: Array Expr <-
    fmapL parseErrorMessage
      $ sequence
      $ (flip runParser $ exprParser <* eof)
      <$> split (Pattern "\n") input
  evalProgram program
