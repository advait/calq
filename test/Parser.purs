module Test.Parser where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.SortedArray as SortedArray
import Effect.Exception (Error)
import Parser (baseUnitParser, compUnitParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (eof)
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), TimeUnit(..))

spec :: Spec Unit
spec = do
  describe "baseUnitParser" do
    it "parses meters" do
      parseTest "meters" (Distance Meters) baseUnitParser
      parseTest "meter" (Distance Meters) baseUnitParser
      parseTest "m" (Distance Meters) baseUnitParser
    it "parses feet" do
      parseTest "feet" (Distance Feet) baseUnitParser
      parseTest "ft" (Distance Feet) baseUnitParser
      parseTest "foot" (Distance Feet) baseUnitParser
    it "parses seconds" do
      parseTest "seconds" (Time Seconds) baseUnitParser
      parseTest "second" (Time Seconds) baseUnitParser
      parseTest "s" (Time Seconds) baseUnitParser
  describe "compUnitParser" do
    it "parses m" do
      let
        expected = compUnit [ Distance Meters ] []
      parseTest "m" expected compUnitParser
      parseTest "(m)" expected compUnitParser
      parseTest "m^1" expected compUnitParser
      parseTest "(m)^1" expected compUnitParser
    it "parses m*ft" do
      let
        expected = compUnit [ Distance Meters, Distance Feet ] []
      parseTest "m*ft" expected compUnitParser
      parseTest "(m)*ft" expected compUnitParser
      parseTest "(m)*(ft)" expected compUnitParser
      parseTest "((m)*(ft))" expected compUnitParser
    it "parses ft/s*m/hr" do
      let
        expected = compUnit [ Distance Feet, Distance Meters ] [ Time Seconds, Time Hours ]
      parseTest "ft/s*m/hr" expected compUnitParser
    it "parses powers" do
      let
        expected = compUnit [ Distance Meters ] [ Time Seconds, Time Seconds ]
      parseTest "m/s^2" expected compUnitParser
      parseTest "(m)/(s)^2" expected compUnitParser
      let
        expected' = compUnit [ Distance Meters, Distance Meters ] []
      parseTest "m^2" expected' compUnitParser
      parseTest "m*m" expected' compUnitParser
      let
        expected'' = compUnit [ Distance Meters, Distance Meters ] [ Time Seconds, Time Seconds ]
      parseTest "(m/s)^2" expected'' compUnitParser
      parseTest "((m)/s)^2" expected'' compUnitParser
      parseTest "((m)/(s))^2" expected'' compUnitParser

parseTest ::
  forall a m.
  Show a =>
  Eq a =>
  MonadThrow Error m =>
  String -> a -> Parser String a -> m Unit
parseTest input expected p = case runParser input (p <* eof) of
  Left err -> fail ("error: " <> show err)
  Right actual -> do
    when (expected /= actual) $ fail $ show actual <> " ≠ " <> show expected

compUnit :: Array BaseUnit -> Array BaseUnit -> CompUnit
compUnit num den = CompUnit { num: SortedArray.sort num, den: SortedArray.sort den }
