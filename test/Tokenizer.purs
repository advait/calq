module Test.Tokenizer where

import Prelude
import Data.Either (Either(..))
import Data.Formatter.Number (format, Formatter(..))
import Data.Int (toNumber)
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (Parser, runParser)
import Tokenizer (TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "Parsing" do
    it "parses integers" do
      quickCheck \(n :: Int) -> case runParser (show n) numberParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)
    it "parses integers with commas" do
      quickCheck \(n :: Int) ->
        let
          formatted = format (Formatter { abbreviations: false, after: 0, before: 10, comma: true, sign: false }) (toNumber n)
        in
          case runParser formatted numberParser of
            Left err -> Failed $ show err
            Right parsed -> parsed === formatted
    it "parses floats" do
      quickCheck \(n :: Number) -> case runParser (show n) numberParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)
    it "parses unknown characters" do
      runParser "~" tokenStreamParser `shouldEqual` Right [ UnknownTk "~" ]

numberParser :: Parser String String
numberParser = f <$> tokenStreamParser
  where
  f :: Array TokenType -> String
  f [ (NumberTk first) ] = first

  f other = "Failed to parse"
