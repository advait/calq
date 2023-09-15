module Tokenizer where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isDecDigit, isHexDigit, isOctDigit, isSpace)
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, joinWith, length)
import Data.String.CodePoints as CodePoints
import Expression (infixNames)
import Parsing (ParseError, Parser, fail, runParser)
import Parsing as Parser
import Parsing.Combinators (choice, try)
import Parsing.String (satisfy, satisfyCodePoint, string)

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
  | UnknownTk String

derive instance eqTokenType :: Eq TokenType

instance showTokenType :: Show TokenType where
  show (WhitespaceTk s) = s
  show NewlineTk = "\n"
  show (PunctuationTk OpenParen) = "("
  show (PunctuationTk CloseParen) = ")"
  show (PunctuationTk Comma) = ","
  show (PunctuationTk Equals) = "="
  show (BaseLiteralTk s) = s
  show (NumberTk s) = s
  show (InfixTk s) = s
  show (ReservedTk UnitTk) = "unit"
  show (ReservedTk AliasTk) = "alias"
  show (ReservedTk PrefixTk) = "prefix"
  show (ReservedTk InTk) = "in"
  show (NameTk s) = s
  show (CommentTk c) = c
  show (UnknownTk c) = c

data Punctuation
  = OpenParen
  | CloseParen
  | Comma
  | Equals

derive instance eqPunctuation :: Eq Punctuation

data ReservedWord
  = UnitTk
  | AliasTk
  | PrefixTk
  | InTk

derive instance eqReservedWord :: Eq ReservedWord

type TokenStream = Array TokenType

tokenStreamParser :: Parser String TokenStream
tokenStreamParser = parser
  where
  -- | Zero or more.
  many :: Parser String CodePoint -> Parser String String
  many p = fromCodePointArray <$> Array.many p

  -- | One or more.
  some :: Parser String CodePoint -> Parser String String
  some p = fromCodePointArray <$> Array.some p

  oneOfChar' :: Array Char -> Parser String Char
  oneOfChar' cs = satisfy (\c -> Array.elem c cs)

  oneOfChar :: Array Char -> Parser String CodePoint
  oneOfChar cs = codePointFromChar <$> (oneOfChar' cs)

  -- | Tries p and falls back to the empty string.
  optional :: Parser String String -> Parser String String
  optional p = (try p) <|> pure ""

  whitespace = WhitespaceTk <$> (some $ oneOfChar [ ' ', '\t' ])

  newline = NewlineTk <$ oneOfChar [ '\n' ]

  punctuation = do
    c <- oneOfChar' [ '(', ')', ',', '=' ]
    case c of
      '(' -> pure $ PunctuationTk OpenParen
      ')' -> pure $ PunctuationTk CloseParen
      ',' -> pure $ PunctuationTk Comma
      '=' -> pure $ PunctuationTk Equals
      _ -> Parser.fail "Unknown punctuation"

  baseLiteral = BaseLiteralTk <$> (hex <|> oct <|> bin)
    where
    hex = do
      prefix <- string "0x" <|> string "0X"
      body <- many $ satisfyCodePoint isHexDigit
      pure $ prefix <> body

    oct = do
      prefix <- string "0o" <|> string "0O"
      body <- many $ satisfyCodePoint isOctDigit
      pure $ prefix <> body

    bin = do
      prefix <- string "0b" <|> string "0B"
      body <- many $ oneOfChar [ '0', '1' ]
      pure $ prefix <> body

  number = NumberTk <$> choice (try <$> [ sci, withCommas, withoutCommas ])
    where
    sci = do
      prefix <- optional $ string "-"
      preDecimal <- many $ satisfyCodePoint isDecDigit
      postDecimal <- optional $ string "." <> (some $ satisfyCodePoint isDecDigit)
      e <- do
        e' <- CodePoints.singleton <$> oneOfChar [ 'e', 'E' ]
        sign <- optional (CodePoints.singleton <$> oneOfChar [ '+', '-' ])
        value <- some $ satisfyCodePoint isDecDigit
        pure $ e' <> sign <> value
      pure $ prefix <> preDecimal <> postDecimal <> e

    withCommas = do
      prefix <- optional $ string "-"
      let
        digitP = CodePoints.singleton <$> satisfyCodePoint isDecDigit
      headGroup <- choice (try <$> [ digitP <> digitP <> digitP, digitP <> digitP, digitP ])
      let
        groupP = do
          comma <- string ","
          body <- digitP <> digitP <> digitP
          pure $ comma <> body
      tailGroups <- joinWith "" <$> Array.some groupP
      postDecimal <-
        optional
          ( do
              dot <- string "."
              decimal <- many $ satisfyCodePoint isDecDigit
              pure $ dot <> decimal
          )
      pure $ prefix <> headGroup <> tailGroups <> postDecimal

    withoutCommas = do
      prefix <- optional $ string "-"
      preDecimal <- many $ satisfyCodePoint isDecDigit
      dot <- optional $ string "."
      postDecimal <- many $ satisfyCodePoint isDecDigit
      if length preDecimal + length postDecimal == 0 then
        fail $ "Invalid number '" <> prefix <> dot <> "'"
      else
        pure $ prefix <> preDecimal <> dot <> postDecimal

  infixP = InfixTk <$> choice (string <$> infixNames)

  -- | Because numbers may start with a leading + or -, we must backtrack for infix tokens.
  numberOrInfix = (try number) <|> infixP

  name = do
    head <- satisfyCodePoint isAlpha <|> oneOfChar [ '_', '"', '\'' ]
    body <- many $ (satisfyCodePoint isAlphaNum <|> oneOfChar [ '_', '"', '\'' ])
    let
      s = CodePoints.singleton head <> body
    case s of
      "unit" -> pure $ ReservedTk UnitTk
      "alias" -> pure $ ReservedTk AliasTk
      "prefix" -> pure $ ReservedTk PrefixTk
      "in" -> pure $ ReservedTk InTk
      _ -> pure $ NameTk s

  comment = do
    start <- string "#"
    body <- many $ satisfyCodePoint ((/=) (codePointFromChar '\n'))
    pure $ CommentTk $ start <> body

  unknown = UnknownTk <$> some (satisfyCodePoint (not <<< isSpace))

  parser =
    Array.many $ oneOf
      $
        [ whitespace
        , newline
        , punctuation
        , baseLiteral
        , numberOrInfix
        , name
        , comment
        , unknown
        ]

tokenize :: String -> Either ParseError TokenStream
tokenize s = runParser s tokenStreamParser

-- | Takes a TokenStream, and splits it based on the NewlineTks.
lines :: TokenStream -> Array TokenStream
lines input = rec [] [] input
  where
  rec :: Array TokenStream -> TokenStream -> TokenStream -> Array TokenStream
  rec acc current input' = case Array.uncons input' of
    Nothing
      | current == [] -> acc
      | otherwise -> acc <> [ current ]
    Just { head, tail }
      | head == NewlineTk -> rec (acc <> [ current ]) [] tail
      | otherwise -> rec acc (current <> [ head ]) tail

removeWhitespaceAndComments :: Array TokenType -> Array TokenType
removeWhitespaceAndComments = Array.filter f
  where
  f (WhitespaceTk _) = false

  f (CommentTk _) = false

  f _ = true
