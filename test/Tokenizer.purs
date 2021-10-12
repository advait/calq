module Test.Tokenizer where

import Prelude
import Data.Either (Either(..))
import Data.Formatter.Number (format, Formatter(..))
import Data.Int (toNumber)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TestUtils (quickCheckParser)
import Text.Parsing.Parser (Parser, runParser)
import Tokenizer (TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "Parsing" do
    it "parses integers" do
      quickCheckParser (\(n :: Int) -> show n) numberParser
    it "parses integers with commas" do
      quickCheckParser
        ( \(n :: Int) ->
            format (Formatter { abbreviations: false, after: 0, before: 10, comma: true, sign: false }) (toNumber n)
        )
        numberParser
    it "parses floats" do
      quickCheckParser (\(n :: Number) -> show n) numberParser
    it "parses unknown characters" do
      runParser "~" tokenStreamParser `shouldEqual` Right [ UnknownTk "~" ]

numberParser :: Parser String String
numberParser = f <$> tokenStreamParser
  where
  f :: Array TokenType -> String
  f [ (NumberTk first) ] = first

  f other = "Failed to parse"
