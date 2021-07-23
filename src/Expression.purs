module Expression where

import Prelude
import Control.Alt ((<|>))
import Data.Array (elem)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either)
import Data.EitherR (fmapL)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (foldl1)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Parsing (bigNumParser, lexeme, spaces)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (choice, optional, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (eof, satisfy, string)

-- | Represents an expression that can be evaluated.
data ParsedExpr
  = Scalar BigNumber
  | Name String
  | Fn1 { name :: String, p1 :: ParsedExpr }
  | Fn2 { name :: String, p1 :: ParsedExpr, p2 :: ParsedExpr }
  | BindPrefix { name :: String, expr :: ParsedExpr }
  | BindRootUnit { name :: String }
  | BindUnit { name :: String, expr :: ParsedExpr }
  | BindAlias { name :: String, target :: String }
  | BindVariable { name :: String, expr :: ParsedExpr }

derive instance eqParsedExpr :: Eq ParsedExpr

instance showParsedExpr :: Show ParsedExpr where
  show (Scalar n) = show n
  show (Name n) = n
  show (Fn1 { name, p1 }) = name <> "(" <> show p1 <> ")"
  show (Fn2 { name, p1, p2 }) = name <> "(" <> show p1 <> ", " <> show p2 <> ")"
  show (BindPrefix { name, expr }) = "prefix " <> name <> " = " <> show expr
  show (BindRootUnit { name }) = "unit " <> name
  show (BindUnit { name, expr }) = "unit " <> name <> " = " <> show expr
  show (BindAlias { name, target }) = "alias " <> name <> " = " <> target
  show (BindVariable { name, expr }) = name <> " = " <> show expr

-- type ParseMetadata = List (Tuple TokenType Position)
-- data TokenType
--   = WhitespaceTk
--   | PunctuationTk
--   | NameTk
--   | NumberTk
--   | InfixTk
--   | InTk
-- type ExprParser a = ParserT String (State ParseMetadata) a
-- -- | Mark the history with the provided token.
-- markT' :: TokenType -> ExprParser Unit
-- markT' tokenType = do
--   pos <- position
--   _ <- lift $ (State.modify $ \history -> (Tuple tokenType pos) : history)
--   pure unit
-- -- | If the provided parser succeeds, mark the history with the provided token.
-- markT :: forall a. TokenType -> ExprParser a -> ExprParser a
-- markT tokenType p = p <* markT' tokenType
-- -- | Parse the given literal string, marking with the given token type.
-- -- | Also strips off whitespace.
-- literal :: TokenType -> String -> ExprParser String
-- literal tokenType str = do
--   s <- string str
--   _ <- markT' tokenType
--   _ <- spaces
--   _ <- markT' WhitespaceTk
--   pure s
-- | Parsing expressions is complicated and requires multiple levels of precedence. Ordinarily we
-- | would just use the builtin buildExprParser but we run into complications because because "1 m"
-- | implies "1*m". The optional lack of an explicit infix operator for multiplication forces us to
-- | break the parser down into multiple levels of priority:
-- | 1. `exprParserBase`: handles the following:
-- |      - Parens: "(42)"
-- |      - Functions: "assertEqual(1, 1)", "reduce(ft)"
-- |      - Binding: "a = 4", "d -> a"
-- |      - Scalars: "1", "3.14"
-- |      - Names: "meters"
-- | 2. `exprParserExponents`: "2^2", "meters^2"
-- | 3. `exprParserImplicitTimes`: "2 m", "100ft"
-- | 4. `exprParser`: Everything else:
-- |      - Other infix operations: "2*2", "4+4"
-- |      - Casting: "3 m/s in mi/hr"
exprParser :: Parser String ParsedExpr
exprParser =
  let
    parenParser :: Parser String ParsedExpr
    parenParser = do
      _ <- lexeme $ string "("
      expr <- lexeme $ exprParser
      _ <- lexeme $ string ")"
      pure expr

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

    bindPrefixParser :: Parser String ParsedExpr
    bindPrefixParser = do
      _ <- lexeme $ string "prefix"
      name <- nameParser
      _ <- lexeme $ string "="
      expr <- exprParser
      pure $ BindPrefix { name, expr }

    bindUnitParser :: Parser String ParsedExpr
    bindUnitParser = do
      _ <- lexeme $ string "unit"
      name <- nameParser
      _ <- lexeme $ string "="
      expr <- exprParser
      pure $ BindUnit { name, expr }

    bindRootUnitParser :: Parser String ParsedExpr
    bindRootUnitParser = do
      _ <- lexeme $ string "unit"
      name <- nameParser
      pure $ BindRootUnit { name }

    bindAliasParser :: Parser String ParsedExpr
    bindAliasParser = do
      _ <- lexeme $ string "alias"
      name <- nameParser
      _ <- lexeme $ string "="
      target <- nameParser
      pure $ BindAlias { name, target }

    bindVariableParser :: Parser String ParsedExpr
    bindVariableParser = do
      name <- nameParser
      _ <- lexeme $ string "="
      expr <- exprParser
      pure $ BindVariable { name, expr }

    scalarParser :: Parser String ParsedExpr
    scalarParser = do
      value <- bigNumParser
      pure $ Scalar value

    nameParser :: Parser String String
    nameParser =
      let
        name =
          lexeme
            $ do
                head <- satisfy startingChar
                tail <- Array.many $ satisfy bodyChar
                pure $ fromCharArray $ Array.cons head tail

        startingChar c = isAlpha c || c `elem` [ '\'', '"' ]

        bodyChar c = isAlphaNum c || c `elem` [ '\'', '"' ]

        notReserved :: String -> Parser String String
        notReserved s =
          if elem s [ "in" ] then
            fail $ "reserved word " <> s
          else
            pure s
      in
        name >>= notReserved

    exprParserBase =
      choice $ try
        <$> [ parenParser
          , fn2Parser
          , fn1Parser
          , bindPrefixParser
          , bindUnitParser
          , bindRootUnitParser
          , bindAliasParser
          , bindVariableParser
          , scalarParser
          , Name <$> nameParser
          ]

    buildFn2 :: String -> ParsedExpr -> ParsedExpr -> ParsedExpr
    buildFn2 name p1 p2 = Fn2 { name, p1, p2 }

    exprParserExponents =
      buildExprParser
        [ [ Infix (buildFn2 <$> (lexeme $ string "^")) AssocLeft ] ]
        (lexeme exprParserBase)

    exprParserImplicitTimes :: Parser String ParsedExpr
    exprParserImplicitTimes = do
      exprs <- NonEmptyArray.some exprParserExponents
      pure $ foldl1 (buildFn2 "*") $ NonEmptyArray.toNonEmpty exprs
  in
    do
      _ <- spaces
      buildExprParser
        [ [ Infix (buildFn2 <$> (lexeme $ string "*")) AssocLeft
          , Infix (buildFn2 <$> (lexeme $ string "/")) AssocLeft
          ]
        , [ Infix (buildFn2 <$> (lexeme $ string "+")) AssocLeft
          , Infix (buildFn2 <$> (lexeme $ string "-")) AssocLeft
          ]
        , [ Infix (buildFn2 <$> (lexeme $ string "in")) AssocLeft ]
        ]
        (lexeme exprParserImplicitTimes)

-- | We attempt to parse line-by-line. There may be some empty or comment-only lines
-- | which are represented by `Nothing`
type Line
  = Maybe ParsedExpr

lineParser :: Parser String Line
lineParser =
  let
    commentParser :: Parser String Unit
    commentParser = do
      _ <- string "#"
      _ <- Array.many $ satisfy ((/=) '\n')
      _ <- eof
      pure unit

    exprParser' :: Parser String Line
    exprParser' = do
      _ <- spaces
      expr <- exprParser
      _ <- optional commentParser
      pure $ Just expr

    noopParser :: Parser String Line
    noopParser = do
      _ <- spaces
      _ <- optional commentParser
      _ <- eof
      pure Nothing
  in
    exprParser' <|> noopParser

parseLines :: String -> Array (Either String Line)
parseLines input =
  fmapL parseErrorMessage
    <$> (flip runParser $ lineParser <* eof)
    <$> String.split (Pattern "\n") input
