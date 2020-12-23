module Test.Interpreter where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as String
import Effect.Exception (Error, error)
import Interpreter (approxEqual, eval, evalProgram', exprParser, initState, valueParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (eof)

spec :: Spec Unit
spec = do
  describe "full form constant values" do
    interpreterTest "1 m in m" "1 m"
    interpreterTest "1 m in ft" "3.28084 ft"
    interpreterTest "4.0 m*m in ft^2" "43.0556 ft^2"
    interpreterTest "4.0 m^2 in ft^2" "43.0556 ft^2"
    interpreterTest "4.0 ft*ft in ft^2" "4.0 ft^2"
    interpreterTest "3.8 lightyears in parsecs" "1.165085 parsecs"
    interpreterTest "27 m^2 in inches*inches" "41850.086378 inches^2"
    interpreterTest "1.0 mi in ft" "5280 feet"
    interpreterTest "2 days in minutes" "2880 minutes"
    interpreterTest "65 miles/hour in km/h" "104.607356652 kilometers/hour"
  describe "omitted values" do
    interpreterTest "42" "42"
    interpreterTest "42 inches" "42 inch"
  describe "strange whitespace" do
    interpreterTest "42   \t" "42"
    interpreterTest "1  \t  m in \t m" "1 m"
  describe "derefernces predefined variables" do
    interpreterTest "c" "299792458 m/s"
    interpreterTest "pi" "3.14159265354"
  describe "full programs" do
    programTest "assertEqual(1, 1.0)"
    programTest "assertEqual(1 m in ft, 3.28084 ft)"
    programTest "d = 1 cm in ft\nassertEqual(d, 0.0328084 ft)"
    programTest basicTest
  describe "multiplication" do
    interpreterTest "2*2" "4"
    interpreterTest "2*2ft" "4ft"
    interpreterTest "2 ft * 2" "4ft"
    describe "simplification" do
      interpreterTest "2 ft *  2 hours/feet" "4 hours"
      interpreterTest "200 m/s * 3 hours" "2160000 meters"
  describe "division" do
    interpreterTest "2ft /2" "1ft"
    programTest "a = 2 ft\nb = 2\nassertEqual(a/b, 1 ft)"
    describe "simplification" do
      interpreterTest "c * 1 minute / 4 lightyears " "4.75320942159e-7"

-- TODO(advait): Move this to its own file BasicTest.calq
basicTest :: String
basicTest =
  String.trim
    """
c
1
assertEqual(1, 1)
assertEqual(1 ft, 1 ft)
assertEqual(1ft, 1 ft)
assertEqual(1ft, 1ft)
d = 1
assertEqual(d, 1.0)
hello = 1e7 feet
assertEqual(hello, 1e+7 ft)
assertEqual(1e7 feet in lightyears, 3.222e-10 lightyears)
assertEqual(2*2,4)
b = 2 ft * 2 ft
assertEqual(b, 4 ft^2)
c = 2ft * 2ft
assertEqual(c, 4ft^2)
assertEqual(2ft * 2ft, 4ft^2)
assertEqual(4 / 2, 2)
"""

interpreterTest :: String -> String -> Spec Unit
interpreterTest input expected =
  it ("converts " <> show input <> " to " <> show expected <> " successfully") do
    input' <- runParserOrFail input (exprParser <* eof)
    expected' <- runParserOrFail expected (valueParser <* eof)
    case evalStateT (eval input') initState of
      Left err -> fail err
      Right actual -> unless (expected' `approxEqual` actual) $ fail $ show actual <> " ≠ " <> show expected'

programTest :: String -> Spec Unit
programTest input =
  it ("runs: " <> (replaceAll (Pattern "\n") (Replacement "\\n") input)) do
    case evalProgram' input of
      Left err -> throwError $ error err
      Right _ -> 1 `shouldEqual` 1

runParserOrFail ::
  forall a m.
  MonadThrow Error m =>
  String -> Parser String a -> m a
runParserOrFail input p = case runParser input p of
  Right res -> pure res
  Left err -> throwError $ error ("Failed to parse " <> show input <> ": " <> show err)
