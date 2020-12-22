module Interpreter where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.State (StateT, lift)
import Data.BigNumber (BigNumber)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Parser (bigNumParser, compUnitParser, lexeme)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (string)
import Units (CompUnit, convertCompUnit)

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

exprParser :: Parser String Expr
exprParser = do
  value@(Tuple _ unit) <- valueParser
  showUnit <- (try $ Just <$> ((lexeme $ string "in") *> compUnitParser)) <|> (pure Nothing)
  pure $ Constant value (fromMaybe unit showUnit)

type InterpreterState
  = Unit

initState :: InterpreterState
initState = unit

-- | Evaluates an expression
eval :: Expr -> StateT InterpreterState (Either String) Value
eval (Constant (Tuple value fromUnit) toUnit) = do
  ratio <- lift $ convertCompUnit fromUnit toUnit
  pure (Tuple (value * ratio) toUnit)
