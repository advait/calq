module Tokenizer where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Char.Unicode (isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit)
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.String (length)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), fst)
import React (ReactElement)
import React.DOM.Props as Props
import React.DOM as DOM
import Text.Parsing.Parser (ParseError(..), Parser, fail, runParser)
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators (choice, try)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (satisfy, string)
import Utils (undefinedLog)

-- | First we parse the input into Tokens to make it easier for subsequent expression parsing.
-- | The tokenizer allows us to more easily handle whitespace subtleties (e.g. "4 m" == "4*m").
data TokenType
  = WhitespaceTk String
  | NewlineTk
  | PunctuationTk Punctuation
  | BaseLiteralTk String
  | NumberTk String
  | InfixTk String
  | ReservedTk ReservedWord
  | NameTk String
  | CommentTk String

derive instance eqTokenType :: Eq TokenType

instance showTokenType :: Show TokenType where
  show (WhitespaceTk s) = s
  show NewlineTk = "\n"
  show (PunctuationTk OpenParen) = "("
  show (PunctuationTk CloseParen) = ")"
  show (PunctuationTk Comma) = ","
  show (BaseLiteralTk s) = s
  show (NumberTk s) = s
  show (InfixTk s) = s
  show (ReservedTk UnitTk) = "unit"
  show (ReservedTk AliasTk) = "alias"
  show (ReservedTk PrefixTk) = "prefix"
  show (ReservedTk InTk) = "in"
  show (NameTk s) = s
  show (CommentTk c) = c

data Punctuation
  = OpenParen
  | CloseParen
  | Comma

derive instance eqPunctuation :: Eq Punctuation

data ReservedWord
  = UnitTk
  | AliasTk
  | PrefixTk
  | InTk

derive instance eqReservedWord :: Eq ReservedWord

type TokenStream
  = Array (Tuple TokenType Position)

tokenStreamParser :: Parser String TokenStream
tokenStreamParser = parser
  where
  -- | Zero or more.
  many :: Parser String Char -> Parser String String
  many p = String.fromCharArray <$> Array.many p

  -- | One or more.
  some :: Parser String Char -> Parser String String
  some p = String.fromCharArray <$> Array.some p

  oneOfChar :: Array Char -> Parser String Char
  oneOfChar cs = satisfy (\c -> Array.elem c cs)

  -- | Tries p and falls back to the empty string.
  optional :: Parser String String -> Parser String String
  optional p = (try p) <|> pure ""

  -- | Annotates the given parser with its start position.
  withPos :: Parser String TokenType -> Parser String (Tuple TokenType Position)
  withPos p = do
    pos <- Parser.position
    parsed <- p
    pure $ Tuple parsed pos

  whitespace = WhitespaceTk <$> (some $ oneOfChar [ ' ', '\t' ])

  newline = NewlineTk <$ oneOfChar [ '\n' ]

  punctuation = do
    c <- oneOfChar [ '(', ')', ',' ]
    case c of
      '(' -> pure $ PunctuationTk OpenParen
      ')' -> pure $ PunctuationTk CloseParen
      ',' -> pure $ PunctuationTk Comma
      _ -> Parser.fail "Unknown punctuation"

  baseLiteral = BaseLiteralTk <$> (hex <|> oct <|> bin)
    where
    hex = do
      prefix <- string "0x" <|> string "0X"
      body <- many $ satisfy isHexDigit
      pure $ prefix <> body

    oct = do
      prefix <- string "0o" <|> string "0O"
      body <- many $ satisfy isOctDigit
      pure $ prefix <> body

    bin = do
      prefix <- string "0b" <|> string "0B"
      body <- many $ oneOfChar [ '0', '1' ]
      pure $ prefix <> body

  number = NumberTk <$> choice (try <$> [ sci, withoutCommas ])
    where
    sci = do
      prefix <- optional (String.singleton <$> oneOfChar [ '+', '-' ])
      preDecimal <- many $ satisfy isDigit
      postDecimal <- optional $ string "." <> (some $ satisfy isDigit)
      e <- do
        e' <- String.singleton <$> oneOfChar [ 'e', 'E' ]
        sign <- optional (String.singleton <$> oneOfChar [ '+', '-' ])
        value <- some $ satisfy isDigit
        pure $ e' <> sign <> value
      pure $ prefix <> preDecimal <> postDecimal <> e

    withoutCommas = do
      prefix <- optional $ String.singleton <$> oneOfChar [ '+', '-' ]
      preDecimal <- many $ satisfy isDigit
      dot <- string "." <|> string ""
      postDecimal <- many $ satisfy isDigit
      if length preDecimal + length postDecimal == 0 then
        fail $ "Invalid number '" <> prefix <> dot <> "'"
      else
        pure $ prefix <> preDecimal <> dot <> postDecimal

    foo = do
      prefix <- optional $ String.singleton <$> oneOfChar [ '+', '-' ]
      pure 2

  infixP = InfixTk <$> String.singleton <$> oneOfChar [ '+', '-', '*', '/', '^' ]

  -- | Because numbers may start with a leading + or -, we must backtrack for infix tokens.
  numberOrInfix = (try number) <|> infixP

  name = do
    head <- satisfy isAlpha <|> oneOfChar [ '_', '"', '\'' ]
    body <- many $ (satisfy isAlphaNum <|> oneOfChar [ '_', '"', '\'' ])
    let
      s = String.singleton head <> body
    case s of
      "unit" -> pure $ ReservedTk UnitTk
      "alias" -> pure $ ReservedTk AliasTk
      "prefix" -> pure $ ReservedTk PrefixTk
      "in" -> pure $ ReservedTk InTk
      _ -> pure $ NameTk s

  comment = do
    start <- string "#"
    body <- many $ satisfy ((/=) '\n')
    pure $ CommentTk $ start <> body

  parser =
    Array.many $ withPos $ oneOf
      $ [ whitespace
        , newline
        , punctuation
        , baseLiteral
        , numberOrInfix
        , name
        , comment
        ]

tokenize :: String -> Either ParseError TokenStream
tokenize s = runParser s tokenStreamParser

highlight :: String -> Either ParseError (Array ReactElement)
highlight s =
  let
    createSpan :: TokenType -> ReactElement
    createSpan (WhitespaceTk w) = DOM.span [ Props.className "token-whitespace" ] [ DOM.text w ]

    createSpan (NewlineTk) = DOM.br [ Props.className "token-newline" ]

    createSpan p@(PunctuationTk _) = DOM.span [ Props.className "token-punctuation" ] [ DOM.text (show p) ]

    createSpan (BaseLiteralTk n) = DOM.span [ Props.className "token-number" ] [ DOM.text n ]

    createSpan (NumberTk n) = DOM.span [ Props.className "token-number" ] [ DOM.text n ]

    createSpan (InfixTk i) = DOM.span [ Props.className "token-infix" ] [ DOM.text i ]

    createSpan w@(ReservedTk _) = DOM.span [ Props.className "token-reserved" ] [ DOM.text (show w) ]

    createSpan (NameTk n) = DOM.span [ Props.className "token-name" ] [ DOM.text n ]

    createSpan (CommentTk c) = DOM.span [ Props.className "token-comment" ] [ DOM.text c ]
  in
    do
      stream :: Array (Tuple TokenType _) <- tokenize s
      pure $ (createSpan <$> fst <$> stream) <> ([ DOM.br [] ])