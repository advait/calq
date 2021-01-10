module Expressions where

import Prelude
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, notFollowedBy, try, (<?>))
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (satisfy, string)
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

-- | `lexeme p` first applies parser `p` and than the `whiteSpace`
-- | parser, returning the value of `p`. Every lexical
-- | token (lexeme) is defined using `lexeme`, this way every parse
-- | starts at a point without white space. Parsers that use `lexeme` are
-- | called *lexeme* parsers in this document.
-- |
-- | The only point where the `whiteSpace` parser should be
-- | called explicitly is the start of the main parser in order to skip
-- | any leading white space.
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
