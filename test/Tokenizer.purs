module Test.Tokenizer where

import Prelude
import Data.Either (Either(..))
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
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
