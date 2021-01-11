module Expressions where

import Prelude
import Control.Alt ((<|>))
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
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (choice, notFollowedBy, optional, try, (<?>))
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (eof, satisfy, string)
import Text.Parsing.Parser.Token as Token
import Utils (bigNum)

-- | We make of pre-existing parsers to make our lives easier.
tokenParser :: Token.TokenParser
tokenParser = Token.makeTokenParser haskellDef

isSpace :: Char -> Boolean
isSpace c
  | c == ' ' = true
  | c == '\t' = true
  | otherwise = false

-- | Consumes zero or more spaces and tabs.
spaces :: Parser String Unit
spaces = void $ Array.many $ satisfy isSpace

-- | `lexeme p` first applies parser `p` and than the `whiteSpace` parser, returning the value of
-- | `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
-- | point without white space. Parsers that use `lexeme` are called *lexeme* parsers in this
-- | document.
-- |
-- | The only point where the `whiteSpace` parser should be called explicitly is the start of the
-- | main parser in order to skip any leading white space.
lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* spaces

-- | Parses a `BigNumber`
bigNumParser :: Parser String BigNumber
bigNumParser =
  lexeme
    $ choice
        [ try $ bigNum <$> show <$> (tokenParser.float)
        , bigNum <$> show <$> (tokenParser.integer)
        ]
    <?> "number"

-- | Represents an expression that can be evaluated. 
data ParsedExpr
  = Scalar BigNumber
  | Name String
  | Fn1 { name :: String, p1 :: ParsedExpr }
  | Fn2 { name :: String, p1 :: ParsedExpr, p2 :: ParsedExpr }
  | BindDerivedUnit { name :: String, expr :: ParsedExpr }
  | BindAlias { name :: String, expr :: ParsedExpr }

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

    bindDerivedUnitParser :: Parser String ParsedExpr
    bindDerivedUnitParser = do
      name <- nameParser
      _ <- lexeme $ string "::"
      expr <- exprParser
      pure $ BindDerivedUnit { name, expr }

    bindAliasParser :: Parser String ParsedExpr
    bindAliasParser = do
      name <- nameParser
      _ <- lexeme $ string "="
      expr <- exprParser
      pure $ BindAlias { name, expr }

    scalarParser :: Parser String ParsedExpr
    scalarParser = do
      value <- bigNumParser
      pure $ Scalar value

    nameParser :: Parser String String
    nameParser =
      let
        reserved =
          notFollowedBy
            $ lexeme
            $ choice
            $ try
            <$> string
            <$> [ "in" ]

        name =
          lexeme
            $ do
                head <- satisfy isAlpha
                tail <- Array.many $ satisfy isAlphaNum
                pure $ fromCharArray $ Array.cons head tail
      in
        reserved *> name

    exprParserBase =
      choice $ try
        <$> [ parenParser
          , fn2Parser
          , fn1Parser
          , bindDerivedUnitParser
          , bindAliasParser
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