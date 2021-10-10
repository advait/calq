module Test.Interpreter where

import Prelude
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Interpreter (approxEqual, eval, execDefinitions, initState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import TestUtils (runParserOrFail)
import TokenParser (eof, tokenExprParser)
import Tokenizer (removeWhitespaceAndComments, tokenStreamParser)
import Utils as Utils

spec :: Spec Unit
spec = do
  describe "Interpreter" do
    describe "full form constant values" do
      interpreterTest "1" "1"
      interpreterTest "c" "c"
    describe "addition and subtraction" do
      interpreterTest "7 + 11" "18"
      interpreterTest "1 - 1" "0"
      interpreterTest "7m - 9m" "-2m"
    -- TODO(advait): "4-4" gets parsed as "4 * (-4)" due to implicit multiplication.
    --interpreterTest "4-4" "0"
    describe "multiplication and division" do
      interpreterTest "1 * m" "m"
      interpreterTest "21 * 2 * m" " 42 * m"
      interpreterTest "ft" "1*ft"
      interpreterTest "2*2ft" "4ft"
      interpreterTest "2 ft * 2" "4ft"
      describe "simplification" do
        interpreterTest "1*m/m" "1"
        interpreterTest "2 ft *  2 hours/feet" "4 hours"
        interpreterTest "200 m/s * 3 hours" "2160000 meters"
        interpreterTest "c * minute" "17987547480 m"
        interpreterTest "c * 1 minute" "17987547480 meters"
        interpreterTest "c * 1 minute / 4 lightyears " "4.75320942159e-7"
    describe "strange whitespace" do
      interpreterTest "42   \t" "42"
      interpreterTest "1  \t  m in \t m" "1 m"
    describe "ft and inch shorthand" do
      interpreterTest "1' + 6\"" "1.5ft"
    describe "bigints" do
      interpreterTest "2^64 + 1" "1.8446744e+19"
      interpreterTest "2^32" "4294967296"
      interpreterTest "2^31" "2147483648"
    describe "basic unit conversions" do
      interpreterTest "m in ft" "3.280839895*ft"
    describe "reduce" do
      interpreterTest "reduce(1)" "1"
      interpreterTest "reduce(1m)" "m"
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
      interpreterTest "2 days in minutes" "2880 minutes"
      interpreterTest "3.8 lightyears in parsecs" "1.165164643 parsecs"
      interpreterTest "1.0 mi in ft" "5280 feet"
      interpreterTest "27 m^2 in inches*inches" "41850.086378 inches^2"
      interpreterTest "65 miles/hour in km/h" "104.607356652 km/hour"
    describe "exponentials" do
      interpreterTest "2^2" "4"
      interpreterTest "4.0 m*m in ft^2" "43.0556 ft^2"
      interpreterTest "1 m^2*m^-2" "1"
      interpreterTest "1 m^2/m^2" "1"
      interpreterTest "4.0 ft*ft in ft^2" "4.0 ft^2"
    describe "derefernces predefined variables" do
      interpreterTest "c" "299792458 m/s"
      interpreterTest "reduce(c)" "299792458 m/s"
      interpreterTest "pi" "3.14159265354"
      interpreterTest "reduce(pi)" "3.14159265354"
    describe "prefixes" do
      interpreterTest "km" "km"
      -- TODO(advait): Support prefix aliases
      --interpreterTest "kilom" "km"
      --interpreterTest "kilometer" "km"
      interpreterTest "reduce(km)" "1000 m"
      interpreterTest "1 mi in km" "1.60934 km"
    describe "full programs" do
      programTest "assertEqual(1, 1)"
    describe "comments" do
      programTest "# Full line comment"
      programTest "assertEqual(1, 1) # Partial line comment"
    describe "InterpreterTest.calq" do
      programTest Utils.interpreterTestFile

interpreterTest :: String -> String -> Spec Unit
interpreterTest input expected =
  it ("converts " <> show input <> " to " <> show expected <> " successfully") do
    inputTokens <- removeWhitespaceAndComments <$> runParserOrFail input tokenStreamParser
    input' <- runParserOrFail inputTokens (tokenExprParser <* eof)
    expectedTokens <- removeWhitespaceAndComments <$> runParserOrFail expected tokenStreamParser
    expected' <- runParserOrFail expectedTokens (tokenExprParser <* eof)
    case Tuple (evalStateT (eval input') initState) (evalStateT (eval expected') initState) of
      Tuple (Left err) _ -> fail err
      Tuple _ (Left err) -> fail err
      Tuple (Right actual) (Right expected'') -> unless (expected'' `approxEqual` actual) $ fail $ show actual <> " ≠ " <> show expected''

programTest :: String -> Spec Unit
programTest input =
  it ("runs: " <> (replaceAll (Pattern "\n") (Replacement "\\n") input)) do
    let
      _ = execDefinitions initState input
    1 `shouldEqual` 1
