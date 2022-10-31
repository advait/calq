module Test.Tokenizer where

import Prelude
import Data.Either (Either(..))
import Data.Formatter.Number (format, Formatter(..))
import Data.Int (toNumber)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TestUtils (quickCheckParser)
import Parsing (Parser, runParser)
import Tokenizer (TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "Parsing" do
    it "parses integers" do
      quickCheckParser (show :: Int -> String) numberParser
    it "parses integers with commas" do
      quickCheckParser
        ( \(n :: Int) ->
            format (Formatter { abbreviations: false, after: 0, before: 10, comma: true, sign: false }) (toNumber n)
        )
        numberParser
    it "parses floats" do
      quickCheckParser (show :: Number -> String) numberParser
    it "parses unknown characters" do
      runParser "~" tokenStreamParser `shouldEqual` Right [ UnknownTk "~" ]
    it "manual tests" do
      runParser "*" tokenStreamParser `shouldEqual` (Right [ InfixTk "*" ])
      runParser "**" tokenStreamParser `shouldEqual` (Right [ InfixTk "*", InfixTk "*" ])
      runParser "-1" tokenStreamParser `shouldEqual` (Right [ NumberTk "-1" ])
      runParser "*-1" tokenStreamParser `shouldEqual` (Right [ InfixTk "*", NumberTk "-1" ])
      runParser "-1*-1" tokenStreamParser `shouldEqual` (Right [ NumberTk "-1", InfixTk "*", NumberTk "-1" ])
      runParser "2+-3" tokenStreamParser `shouldEqual` (Right [ NumberTk "2", InfixTk "+", NumberTk "-3" ])

numberParser :: Parser String String
numberParser = f <$> tokenStreamParser
  where
  f :: Array TokenType -> String
  f [ (NumberTk first) ] = first

  f _ = "Failed to parse"
