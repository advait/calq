module Test.Interpreter where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Interpreter (Value, eval, exprParser, initState, valueParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (eof)
import Units (bigNum)

spec :: Spec Unit
spec = do
  describe "interpreter" do
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

interpreterTest :: String -> String -> Spec Unit
interpreterTest input expected =
  it ("converts " <> show input <> " to " <> show expected <> " successfully") do
    input' <- runParserOrFail input (exprParser <* eof)
    expected' <- runParserOrFail expected (valueParser <* eof)
    case evalStateT (eval input') initState of
      Left err -> fail err
      Right actual -> unless (expected' `approxEqual` actual) $ fail $ show actual <> " ≠ " <> show expected'

approxEqual :: Value -> Value -> Boolean
approxEqual (Tuple v1 u1) (Tuple v2 u2) = (u1 == u2) && (abs (v1 - v2) / v1) < (bigNum "1e-5")

runParserOrFail ::
  forall a m.
  MonadThrow Error m =>
  String -> Parser String a -> m a
runParserOrFail input p = case runParser input p of
  Right res -> pure res
  Left err -> throwError $ error ("Failed to parse " <> show input <> ": " <> show err)
