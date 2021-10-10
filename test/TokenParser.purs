module Test.TokenParser where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Expression (ParsedExpr)
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (ParseError, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (Punctuation(..), TokenType(..), tokenStreamParser)

spec :: Spec Unit
spec = do
  describe "TokenParser" do
    it "parses integers" do
      quickCheck \(n :: Int) -> case execP (show n) of
        Left err -> Failed $ "Failed to parse: " <> show n <> "\nError: " <> (show err)
        Right parsed -> show parsed === show n
    it "parses floats" do
      quickCheck \(n :: Number) -> case execP (show n) of
        Left err -> Failed $ show n
        Right parsed -> show parsed === show n
    it "parses infix" do
      quickCheck \(Tuple (a :: Int) (b :: Int)) ->
        let
          s = (show a <> "*" <> show b)
        in
          case execP s of
            Left err -> Failed $ show s
            Right parsed -> show parsed === s
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

execP :: String -> Either ParseError ParsedExpr
execP input = do
  tokenStream <- runParser input tokenStreamParser
  runParser tokenStream tokenExprParser
