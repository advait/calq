module Test.Parsing where

import Prelude
import Data.Either (Either(..))
import Data.Number.Format as Format
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (Parser(..), fail, runParser)
import Tokenizer (TokenType(..), tokenStreamParser)
import Utils (parseBigNumber)

spec :: Spec Unit
spec = do
  describe "Parsing" do
    it "parses integers" do
      quickCheck \(n :: Int) -> case runParser (show n) numberParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)
    it "parses floats" do
      quickCheck \(n :: Number) -> case runParser (show n) numberParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)

numberParser :: Parser String String
numberParser = f <$> tokenStreamParser
  where
  f :: Array TokenType -> String
  f [ (NumberTk first) ] = first

  f other = "Failed to parse"
