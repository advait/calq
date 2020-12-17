module Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Array (many, cons)
import Data.Foldable (foldl)
import Data.SortedArray as SortedArray
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token as Token
import Units (BaseUnit(..), DistanceUnit(..), TimeUnit(..), CompUnit(..))

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
  let
    start :: Parser String (CompUnit -> CompUnit)
    start = do
      parsedUnit <- baseUnitParser
      pure (\(CompUnit { num, den }) -> CompUnit { num: SortedArray.insert parsedUnit num, den: den })

    timesParser :: Parser String (CompUnit -> CompUnit)
    timesParser = do
      _ <- string "*"
      parsedUnit <- baseUnitParser
      pure (\(CompUnit { num, den }) -> CompUnit { num: SortedArray.insert parsedUnit num, den: den })

    divParser :: Parser String (CompUnit -> CompUnit)
    divParser = do
      _ <- string "/"
      parsedUnit <- baseUnitParser
      pure (\(CompUnit { num, den }) -> CompUnit { num: num, den: SortedArray.insert parsedUnit den })

    tailItems :: Parser String (Array (CompUnit -> CompUnit))
    tailItems = many (timesParser <|> divParser)

    items :: Parser String (Array (CompUnit -> CompUnit))
    items = cons <$> start <*> tailItems

    mergeCompUnits :: CompUnit -> (CompUnit -> CompUnit) -> CompUnit
    mergeCompUnits c f = f c
  in
    do
      parsedItems <- items
      pure $ foldl mergeCompUnits mempty parsedItems
