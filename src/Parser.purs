module Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.SortedArray as SortedArray
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token as Token
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), TimeUnit(..), div, pow, times)

tokenParser :: Token.TokenParser
tokenParser = Token.makeTokenParser haskellDef

baseUnitParser :: Parser String BaseUnit
baseUnitParser =
  let
    distanceParser :: Parser String BaseUnit
    distanceParser =
      choice
        [ (choice $ string <$> [ "meters", "meter", "m" ]) *> pure (Distance Meters)
        , (choice $ string <$> [ "kilometers", "kilometer", "km" ]) *> pure (Distance Kilometers)
        , (choice $ string <$> [ "ft", "feet", "foot" ]) *> pure (Distance Feet)
        ]

    timeParser :: Parser String BaseUnit
    timeParser =
      choice
        [ (choice $ string <$> [ "seconds", "second", "s" ]) *> pure (Time Seconds)
        , (choice $ string <$> [ "hours", "hour", "hr", "h" ]) *> pure (Time Hours)
        ]
  in
    choice
      [ distanceParser, timeParser
      ]

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
