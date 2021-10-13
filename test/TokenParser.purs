module Test.TokenParser where

import Prelude
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Expression (Expr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TestUtils (quickCheckParser)
import Text.Parsing.Parser (Parser, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "TokenParser" do
    it "parses integers" do
      quickCheckParser (show :: Int -> String) chainP
    it "parses floats" do
      quickCheckParser (show :: Number -> String) chainP
    it "parses infix" do
      quickCheckParser (\(Tuple (a :: Int) (b :: Int)) -> (show a <> "*" <> show b)) chainP
      quickCheckParser (\(Tuple (a :: Int) (b :: Int)) -> (show a <> "-" <> show b)) chainP
    it "parses arbitrary expressions" do
      quickCheckParser (show :: Expr -> String) chainP
    it "handles eof" do
      (runParser [] eof) `shouldEqual` (Right unit)
    it "handles manual numbers" do
      (show <$> runParser [ NumberTk "1" ] tokenExprParser) `shouldEqual` Right "1"
    it "handles function forms" do
      assertParse "reduce(1)"
    it "handles names" do
      assertParse "hello"
      assertParse "reduce"
    -- it "handles bignums properly" do
    -- (show <$> execP "0b101010101010101010101010101010101010101010101010101010101010101010") `shouldEqual` (Right "49191317529892137642")
    -- (show <$> execP "0B101010101010101010101010101010101010101010101010101010101010101010") `shouldEqual` (Right "49191317529892137642")
    -- (show <$> execP "0x1234567890abcdef1234567890abcdef") `shouldEqual` (Right "24197857200151252728969465429440056815")
    -- (show <$> execP "0X1234567890abcdef1234567890abcdef") `shouldEqual` (Right "24197857200151252728969465429440056815")
    -- (show <$> execP "0o56123423423423523452345234512341256245365236462452345642") `shouldEqual` (Right "269870138443162641462903354048649622586931538414498")
    -- (show <$> execP "0O56123423423423523452345234512341256245365236462452345642") `shouldEqual` (Right "269870138443162641462903354048649622586931538414498")
    it "manual tests" do
      assertParse "2+-3"
      assertParse "(-465650+-765144)+(-774230*-317464)"

-- | Runs the tokenStreamParser and then tokenExprParser in sequence.
chainP :: Parser String String
chainP = do
  tokenStream <- tokenStreamParser
  case runParser tokenStream tokenExprParser of
    Left err -> throwError err
    Right foo -> pure $ show foo

assertParse :: String -> Aff Unit
assertParse input = runParser input chainP `shouldEqual` Right input
