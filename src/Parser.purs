module Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, cons)
import Data.Foldable (foldl)
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
        , (choice $ string <$> [ "ft", "feet", "foot" ]) *> pure (Distance Feet)
        ]

    timeParser :: Parser String BaseUnit
    timeParser =
      choice
        [ (choice $ string <$> [ "seconds", "second", "s" ]) *> pure (Time Seconds)
        , (choice $ string <$> [ "hours", "hour", "h" ]) *> pure (Time Hours)
        ]
  in
    choice
      [ distanceParser, timeParser
      ]

compUnitParser :: Parser String CompUnit
compUnitParser =
  fix \compUnitParser' ->
    let
      item :: Parser String (CompUnit -> CompUnit)
      item =
        let
          paren = do
            _ <- string "("
            c2 <- compUnitParser'
            _ <- string ")"
            pure (\c1 -> c1 `times` c2)

          baseItem = do
            parsedUnit <- baseUnitParser
            pure (\(CompUnit { num, den }) -> CompUnit { num: SortedArray.insert parsedUnit num, den: den })
        in
          choice [ paren, baseItem ]

      timesParser :: Parser String (CompUnit -> CompUnit)
      timesParser = do
        _ <- string "*"
        c2 <- item
        pure (\c1 -> c1 `times` (c2 mempty))

      divParser :: Parser String (CompUnit -> CompUnit)
      divParser = do
        _ <- string "/"
        c2 <- item
        pure (\c1 -> c1 `div` (c2 mempty))

      powParser :: Parser String (CompUnit -> CompUnit)
      powParser = do
        base <- item
        _ <- string "^"
        exp <- tokenParser.integer
        pure (\c1 -> c1 `times` ((base mempty) `pow` exp))

      tailItems :: Parser String (Array (CompUnit -> CompUnit))
      tailItems = many (timesParser <|> divParser <|> powParser)

      items :: Parser String (Array (CompUnit -> CompUnit))
      items = cons <$> item <*> tailItems

      mergeCompUnits :: CompUnit -> (CompUnit -> CompUnit) -> CompUnit
      mergeCompUnits c f = f c
    in
      do
        parsedItems <- items
        pure $ foldl mergeCompUnits mempty parsedItems
