module Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many)
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic
import Data.SortedArray as SortedArray
import Data.String (length)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice, try, (<?>))
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (space)
import Text.Parsing.Parser.Token as Token
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), MassUnit(..), TimeUnit(..), div, pow, times)

tokenParser :: Token.TokenParser
tokenParser = Token.makeTokenParser haskellDef

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

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

-- | Parses a `BigNumber`
bigNumParser :: Parser String BigNumber
bigNumParser =
  lexeme
    $ choice
        [ try $ bigNum <$> show <$> (tokenParser.float)
        , bigNum <$> show <$> (tokenParser.integer)
        ]
    <?> "number"

isSpace :: Char -> Boolean
isSpace c
  | c == ' ' = true
  | c == '\t' = true
  | otherwise = false

-- | Consumes zero or more spaces and tabs.
spaces :: Parser String Unit
spaces = void $ many $ space

baseUnitParser :: Parser String BaseUnit
baseUnitParser =
  let
    mappings :: Array (Tuple String BaseUnit)
    mappings =
      Array.reverse
        $ Array.sortWith (\(Tuple s _) -> length s)
        $ [ Tuple "meters" $ Distance Meters
          , Tuple "meter" $ Distance Meters
          , Tuple "m" $ Distance Meters
          , Tuple "centimeters" $ Distance Centimeters
          , Tuple "centimeter" $ Distance Centimeters
          , Tuple "cm" $ Distance Centimeters
          , Tuple "millimeters" $ Distance Millimeters
          , Tuple "millimeter" $ Distance Millimeters
          , Tuple "mm" $ Distance Millimeters
          , Tuple "micrometers" $ Distance Micrometers
          , Tuple "micrometer" $ Distance Micrometers
          , Tuple "μm" $ Distance Micrometers
          , Tuple "um" $ Distance Micrometers
          , Tuple "nanometers" $ Distance Nanometers
          , Tuple "nanometer" $ Distance Nanometers
          , Tuple "nm" $ Distance Nanometers
          , Tuple "kilometers" $ Distance Kilometers
          , Tuple "kilometer" $ Distance Kilometers
          , Tuple "km" $ Distance Kilometers
          , Tuple "feet" $ Distance Feet
          , Tuple "foot" $ Distance Feet
          , Tuple "ft" $ Distance Feet
          , Tuple "'" $ Distance Feet
          , Tuple "inches" $ Distance Inches
          , Tuple "inch" $ Distance Inches
          , Tuple "\"" $ Distance Inches
          , Tuple "yards" $ Distance Yards
          , Tuple "yard" $ Distance Yards
          , Tuple "yd" $ Distance Yards
          , Tuple "miles" $ Distance Miles
          , Tuple "mile" $ Distance Miles
          , Tuple "mi" $ Distance Miles
          , Tuple "lightyears" $ Distance Lightyears
          , Tuple "lightyear" $ Distance Lightyears
          , Tuple "parsecs" $ Distance Parsecs
          , Tuple "parsec" $ Distance Parsecs
          , Tuple "seconds" $ Time Seconds
          , Tuple "second" $ Time Seconds
          , Tuple "s" $ Time Seconds
          , Tuple "minutes" $ Time Minutes
          , Tuple "minute" $ Time Minutes
          , Tuple "min" $ Time Minutes
          , Tuple "hours" $ Time Hours
          , Tuple "hour" $ Time Hours
          , Tuple "hr" $ Time Hours
          , Tuple "h" $ Time Hours
          , Tuple "days" $ Time Days
          , Tuple "day" $ Time Days
          , Tuple "months" $ Time Months
          , Tuple "month" $ Time Months
          , Tuple "years" $ Time Years
          , Tuple "year" $ Time Years
          , Tuple "grams" $ Mass Grams
          , Tuple "gram" $ Mass Grams
          , Tuple "g" $ Mass Grams
          , Tuple "milligrams" $ Mass Milligrams
          , Tuple "milligram" $ Mass Milligrams
          , Tuple "mg" $ Mass Milligrams
          , Tuple "micrograms" $ Mass Micrograms
          , Tuple "microgram" $ Mass Micrograms
          , Tuple "μg" $ Mass Micrograms
          , Tuple "ug" $ Mass Micrograms
          , Tuple "nanograms" $ Mass Nanograms
          , Tuple "nanogram" $ Mass Nanograms
          , Tuple "ng" $ Mass Nanograms
          , Tuple "kilograms" $ Mass Kilograms
          , Tuple "kilogram" $ Mass Kilograms
          , Tuple "kg" $ Mass Kilograms
          , Tuple "pounds" $ Mass Pounds
          , Tuple "pound" $ Mass Pounds
          , Tuple "lbs" $ Mass Pounds
          , Tuple "lb" $ Mass Pounds
          , Tuple "tons" $ Mass Tons
          , Tuple "ton" $ Mass Tons
          , Tuple "ounces" $ Mass Ounces
          , Tuple "ounce" $ Mass Ounces
          , Tuple "oz" $ Mass Ounces
          ]
  in
    choice
      $ (\(Tuple s unit) -> (string s *> pure unit))
      <$> mappings

compUnitParser :: Parser String CompUnit
compUnitParser =
  fix \compUnitParser' ->
    let
      -- | Attempt to greedily apply powers
      powOrIdentity :: CompUnit -> Parser String CompUnit
      powOrIdentity p1 = ((pow p1) <$> (lexeme $ string "^" *> tokenParser.integer)) <|> pure p1

      -- | Either a `BaseUnit` or a `CompUnit` wrapped in parens.
      item :: Parser String CompUnit
      item =
        choice
          [ (lexeme $ string "(") *> compUnitParser' <* (lexeme $ string ")")
          , (\u -> CompUnit { num: SortedArray.singleton u, den: mempty }) <$> baseUnitParser
          ]
          >>= powOrIdentity

      operators :: Parser String (CompUnit -> CompUnit)
      operators =
        choice
          [ (flip times) <$> (lexeme $ string "*" *> item)
          , (flip div) <$> (lexeme $ string "/" *> item)
          ]

      -- | Attempt to repeatedly apply operators left to right
      chainOperators :: CompUnit -> Parser String CompUnit
      chainOperators acc =
        ( do
            op <- operators
            chainOperators (op acc)
        )
          <|> pure acc -- If we can't apply additional operators, fall back to acc
    in
      lexeme
        $ do
            start <- item <|> pure mempty
            chainOperators start
