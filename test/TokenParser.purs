module Test.TokenParser where

import Prelude
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Expression (ParsedExpr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TestUtils (quickCheckParser)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (Punctuation(..), TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "TokenParser" do
    it "parses integers" do
      quickCheckParser (show :: Int -> String) chainP
    it "parses floats" do
      quickCheckParser (show :: Number -> String) chainP
    it "parses infix" do
      quickCheckParser (\(Tuple (a :: Int) (b :: Int)) -> (show a <> "*" <> show b)) chainP
    it "handles eof" do
      (runParser [] eof) `shouldEqual` (Right unit)
    it "handles manual numbers" do
      (show <$> runParser [ NumberTk "1" ] tokenExprParser) `shouldEqual` Right "1"
    it "handles function forms" do
      runParser "reduce(1)" tokenStreamParser `shouldEqual` (Right [ NameTk "reduce", PunctuationTk OpenParen, NumberTk "1", PunctuationTk CloseParen ])
      (show <$> runParser [ NameTk "reduce", PunctuationTk OpenParen, NumberTk "1", PunctuationTk CloseParen ] tokenExprParser) `shouldEqual` Right "reduce(1)"
      (show <$> execP "reduce(1)") `shouldEqual` (Right "reduce(1)")
    it "handles names" do
      (show <$> execP "hello") `shouldEqual` Right "hello"
      (show <$> execP "reduce") `shouldEqual` Right "reduce"
    -- it "handles bignums properly" do
    -- (show <$> execP "0b101010101010101010101010101010101010101010101010101010101010101010") `shouldEqual` (Right "49191317529892137642")
    -- (show <$> execP "0B101010101010101010101010101010101010101010101010101010101010101010") `shouldEqual` (Right "49191317529892137642")
    -- (show <$> execP "0x1234567890abcdef1234567890abcdef") `shouldEqual` (Right "24197857200151252728969465429440056815")
    -- (show <$> execP "0X1234567890abcdef1234567890abcdef") `shouldEqual` (Right "24197857200151252728969465429440056815")
    -- (show <$> execP "0o56123423423423523452345234512341256245365236462452345642") `shouldEqual` (Right "269870138443162641462903354048649622586931538414498")
    -- (show <$> execP "0O56123423423423523452345234512341256245365236462452345642") `shouldEqual` (Right "269870138443162641462903354048649622586931538414498")
    it "manual tests" do
      (show <$> execP "1") `shouldEqual` (Right "1")
      (show <$> execP "-1") `shouldEqual` (Right "-1")
      (show <$> execP "463581") `shouldEqual` (Right "463581")
      runParser "*" tokenStreamParser `shouldEqual` (Right [ InfixTk "*" ])
      runParser "**" tokenStreamParser `shouldEqual` (Right [ InfixTk "*", InfixTk "*" ])
      runParser "-1" tokenStreamParser `shouldEqual` (Right [ NumberTk "-1" ])
      runParser "*-1" tokenStreamParser `shouldEqual` (Right [ InfixTk "*", NumberTk "-1" ])
      runParser "-1*-1" tokenStreamParser `shouldEqual` (Right [ NumberTk "-1", InfixTk "*", NumberTk "-1" ])
      (show <$> execP "1*1") `shouldEqual` (Right "1*1")

chainP :: Parser String String
chainP = do
  tokenStream <- tokenStreamParser
  case runParser tokenStream tokenExprParser of
    Left err -> throwError err
    Right foo -> pure $ show foo

execP :: String -> Either ParseError ParsedExpr
execP input = do
  tokenStream <- runParser input tokenStreamParser
  runParser tokenStream tokenExprParser
