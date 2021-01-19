module Parsing where

import Prelude
import Control.MonadPlus ((<|>))
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.Char.Unicode (isDigit, isHexDigit, isOctDigit)
import Data.Foldable as Foldable
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic
import Data.String.CodeUnits as String
import Data.String.Common (toLower, toUpper)
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (try, (<??>))
import Text.Parsing.Parser.String (oneOf, satisfy, string)

-- | Matches the given character parser zero or more times.
zeroOrMore :: Parser String Char -> Parser String String
zeroOrMore p = String.fromCharArray <$> Array.many p

-- | Matches the given character parser one or more times.
oneOrMore :: Parser String Char -> Parser String String
oneOrMore p = String.fromCharArray <$> Array.some p

-- | Matches the given parser or matches "" if it fails.
optional :: Parser String String -> Parser String String
optional p = p <|> string ""

-- | Matches spaces and tabs but not other whitespace like newlines.
isSpace :: Char -> Boolean
isSpace c
  | c == ' ' = true
  | c == '\t' = true
  | otherwise = false

-- | Consumes zero or more spaces and tabs.
spaces :: forall m. Monad m => ParserT String m Unit
spaces = "whitespace" <??> (void $ Array.many $ satisfy isSpace)

-- | `lexeme p` first applies parser `p` and than the `whiteSpace` parser, returning the value of
-- | `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
-- | point without white space. Parsers that use `lexeme` are called *lexeme* parsers in this
-- | document.
-- |
-- | The only point where the `whiteSpace` parser should be called explicitly is the start of the
-- | main parser in order to skip any leading white space.
lexeme :: forall a m. Monad m => ParserT String m a -> ParserT String m a
lexeme p = p <* spaces

-- | Create a constant `BigNumber` from a `String`.
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

-- | Parses a `BigNumber` including decimal and floating point. Supports prefixed negative sign but
-- | no prefixed positive sign.
bigNumParser :: Parser String BigNumber
bigNumParser = "number" <??> bigNum <$> lexeme floatOrInt
  where
  floatOrInt = (try nonStandardIntParser) <|> floatParser

-- | Parses nonstandard integer formats (binary, hex, oct). Decimal parsing is handled by
-- | floatParser.
nonStandardIntParser :: Parser String String
nonStandardIntParser = Foldable.oneOf $ try <$> negativePrefix <$> [ binary, hex, oct ]
  where
  negativePrefix next = (optional $ string "-") <> next

  binary = bothCases "0b" <> (oneOrMore $ oneOf [ '0', '1' ])

  hex = bothCases "0x" <> (oneOrMore $ satisfy isHexDigit)

  oct = bothCases "0o" <> (oneOrMore $ satisfy isOctDigit)

  -- | Attempts to match both lower and upper case versions of the string. Does not mean case
  -- | insensitive.
  bothCases :: String -> Parser String String
  bothCases s = (try $ string $ toLower s) <|> (string $ toUpper s)

-- | Parses floating forms including exponentials. NaN, +/-Infinity are not supported.
floatParser :: Parser String String
floatParser = gt1 <|> lt1
  where
  -- | Case where something preceeds the decimal.
  gt1 =
    (optional $ string "-")
      <> (oneOrMore $ satisfy isDigit)
      <> (optional $ string "." <> (zeroOrMore $ satisfy isDigit))
      <> optional exp

  -- | Case where nothing preceeds the decimal but something comes after the decimal.
  lt1 =
    (optional $ string "-")
      <> (zeroOrMore $ satisfy isDigit)
      <> (optional $ string "." <> (oneOrMore $ satisfy isDigit))
      <> optional exp

  exp = e <> sign <> (oneOrMore $ satisfy isDigit)
    where
    e = string "e" <|> string "E"

    sign = optional $ string "+" <|> string "-"
