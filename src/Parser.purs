module Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.SortedArray as SortedArray
import Data.String.CodeUnits as String
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token as Token
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), TimeUnit(..), MassUnit(..), div, pow, times)

tokenParser :: Token.TokenParser
tokenParser = Token.makeTokenParser haskellDef

baseUnitParser :: Parser String BaseUnit
baseUnitParser =
  let
    distanceParser :: Parser String BaseUnit
    distanceParser =
      choice
        [ (choice $ string <$> [ "meters", "meter", "m" ]) *> pure (Distance Meters)
        , (choice $ string <$> [ "centimeters", "centimeter", "cm" ]) *> pure (Distance Centimeters)
        , (choice $ string <$> [ "millimeters", "millimeter", "mm" ]) *> pure (Distance Millimeters)
        , (choice $ string <$> [ "micrometers", "micrometer", "μm", "um" ]) *> pure (Distance Micrometers)
        , (choice $ string <$> [ "nanometers", "nanometer", "nm" ]) *> pure (Distance Nanometers)
        , (choice $ string <$> [ "kilometers", "kilometer", "km" ]) *> pure (Distance Kilometers)
        , (choice $ string <$> [ "feet", "foot", "ft", "'" ]) *> pure (Distance Feet)
        , (choice $ string <$> [ "inches", "inch", "in", String.fromCharArray [ '"' ] ]) *> pure (Distance Inches)
        , (choice $ string <$> [ "yards", "yard", "yd" ]) *> pure (Distance Yards)
        , (choice $ string <$> [ "miles", "mile", "mi" ]) *> pure (Distance Miles)
        , (choice $ string <$> [ "lightyears", "lightyear" ]) *> pure (Distance Lightyears)
        , (choice $ string <$> [ "parsecs", "parsec" ]) *> pure (Distance Parsecs)
        ]

    timeParser :: Parser String BaseUnit
    timeParser =
      choice
        [ (choice $ string <$> [ "seconds", "second", "s" ]) *> pure (Time Seconds)
        , (choice $ string <$> [ "minutes", "minute", "min" ]) *> pure (Time Minutes)
        , (choice $ string <$> [ "hours", "hour", "hr", "h" ]) *> pure (Time Hours)
        , (choice $ string <$> [ "days", "day" ]) *> pure (Time Days)
        , (choice $ string <$> [ "months", "month" ]) *> pure (Time Months)
        , (choice $ string <$> [ "years", "year" ]) *> pure (Time Years)
        ]

    massParser :: Parser String BaseUnit
    massParser =
      choice
        [ (choice $ string <$> [ "grams", "gram", "g" ]) *> pure (Mass Grams)
        , (choice $ string <$> [ "milligrams", "milligram", "mg" ]) *> pure (Mass Milligrams)
        , (choice $ string <$> [ "micrograms", "microgram", "μg", "ug" ]) *> pure (Mass Micrograms)
        , (choice $ string <$> [ "nanograms", "nanogram", "ng" ]) *> pure (Mass Nanograms)
        , (choice $ string <$> [ "kilograms", "kilogram", "kg" ]) *> pure (Mass Kilograms)
        , (choice $ string <$> [ "pounds", "pound", "lbs", "lb" ]) *> pure (Mass Pounds)
        , (choice $ string <$> [ "tons", "ton" ]) *> pure (Mass Tons)
        , (choice $ string <$> [ "ounces", "ounce", "oz" ]) *> pure (Mass Ounces)
        ]
  in
    choice [ distanceParser, timeParser, massParser ]

compUnitParser :: Parser String CompUnit
compUnitParser =
  fix \compUnitParser' ->
    let
      -- | Attempt to greedily apply powers
      powOrIdentity :: CompUnit -> Parser String CompUnit
      powOrIdentity p1 = ((pow p1) <$> (string "^" *> tokenParser.integer)) <|> pure p1

      -- | Either a `BaseUnit` or a `CompUnit` wrapped in parens.
      item :: Parser String CompUnit
      item =
        choice
          [ string "(" *> compUnitParser' <* string ")"
          , (\u -> CompUnit { num: SortedArray.singleton u, den: mempty }) <$> baseUnitParser
          ]
          >>= powOrIdentity

      operators :: Parser String (CompUnit -> CompUnit)
      operators =
        choice
          [ (flip times) <$> (string "*" *> item)
          , (flip div) <$> (string "/" *> item)
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
      do
        start <- item <|> pure mempty
        chainOperators start
