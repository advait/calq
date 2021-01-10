module Test.Advaita where

import Prelude
import Advaita (approxEqual, eval, initState)
import Expressions (exprParser)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (eof)

spec :: Spec Unit
spec = do
  describe "Interpreter" do
    describe "full form constant values" do
      interpreterTest "1" "1"
      interpreterTest "c" "c"
    describe "multiplication and division" do
      interpreterTest "1 * m" "m"
      interpreterTest "21 * 2 * m" " 42 * m"
      interpreterTest "1*m/m" "1"
      interpreterTest "ft" "1*ft"
    describe "basic unit conversions" do
      interpreterTest "m in ft" "3.280839895*ft"
    describe "reduce" do
      interpreterTest "reduce(1)" "1"
      interpreterTest "reduce(ft)" "0.3048 * m"
      interpreterTest "reduce(ft * m)" "0.3048 * m * m"
      interpreterTest "reduce(ft / m)" "0.3048"
      interpreterTest "reduce(1 / ft)" "3.28084 / m"
      interpreterTest "reduce(m / ft)" "3.28084"
    describe "convertible units are automatically merged together" do
      interpreterTest "m * ft" "0.3048 * m * m"
      interpreterTest "m / ft" "3.28084"
    describe "implicit multiplciation" do
      interpreterTest "1m" "1*m"
      interpreterTest "(4*4)m" "16m"
    describe "order of operations" do
      interpreterTest "3m/3m" "1"
      interpreterTest "3m/3*m" "m*m"
    describe "casting" do
      interpreterTest "1 m in ft" "3.28084 ft"
      interpreterTest "4.0 m*m in ft*ft" "43.0556 ft*ft"

--   interpreterTest "4.0 m^2 in ft^2" "43.0556 ft^2"
--   interpreterTest "4.0 ft*ft in ft^2" "4.0 ft^2"
--   interpreterTest "3.8 lightyears in parsecs" "1.165085 parsecs"
--   interpreterTest "27 m^2 in inches*inches" "41850.086378 inches^2"
--   interpreterTest "1.0 mi in ft" "5280 feet"
--   interpreterTest "2 days in minutes" "2880 minutes"
--   interpreterTest "65 miles/hour in km/h" "104.607356652 kilometers/hour"
-- describe "omitted values" do
--   interpreterTest "42" "42"
--   interpreterTest "42 inches" "42 inch"
-- describe "strange whitespace" do
--   interpreterTest "42   \t" "42"
--   interpreterTest "1  \t  m in \t m" "1 m"
-- describe "derefernces predefined variables" do
--   interpreterTest "c" "299792458 m/s"
--   interpreterTest "pi" "3.14159265354"
-- describe "multiplication" do
--   interpreterTest "2*2" "4"
--   interpreterTest "2*2ft" "4ft"
--   interpreterTest "2 ft * 2" "4ft"
--   describe "simplification" do
--     interpreterTest "2 ft *  2 hours/feet" "4 hours"
--     interpreterTest "200 m/s * 3 hours" "2160000 meters"
-- describe "division" do
--   interpreterTest "2ft /2" "1ft"
--   describe "simplification" do
--     interpreterTest "c * 1 minute / 4 lightyears " "4.75320942159e-7"
-- -------------------------------------------------
interpreterTest :: String -> String -> Spec Unit
interpreterTest input expected =
  it ("converts " <> show input <> " to " <> show expected <> " successfully") do
    input' <- runParserOrFail input (exprParser <* eof)
    expected' <- runParserOrFail expected (exprParser <* eof)
    case Tuple (evalStateT (eval input') initState) (evalStateT (eval expected') initState) of
      Tuple (Left err) _ -> fail err
      Tuple _ (Left err) -> fail err
      Tuple (Right actual) (Right expected'') -> unless (expected'' `approxEqual` actual) $ fail $ show actual <> " ≠ " <> show expected''

runParserOrFail ::
  forall a m.
  MonadThrow Error m =>
  String -> Parser String a -> m a
runParserOrFail input p = case runParser input p of
  Right res -> pure res
  Left err -> throwError $ error ("Failed to parse " <> show input <> ": " <> show err)

-- programTest :: String -> Spec Unit
-- programTest input =
--   it ("runs: " <> (replaceAll (Pattern "\n") (Replacement "\\n") input)) do
--     case evalProgram' input of
--       Left err -> throwError $ error err
--       Right _ -> 1 `shouldEqual` 1
