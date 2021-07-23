module Test.TokenParser where

import Prelude
import Data.Either (Either(..))
import Expression (ParsedExpr(..))
import Parsing as Parsing
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (ParseError, runParser)
import TokenParser (tokenExprParser)
import Tokenizer (tokenStreamParser)
import Utils (parseBigNumber)

spec :: Spec Unit
spec = do
  describe "TokenParser" do
    it "parses integers" do
      quickCheck \(n :: Int) -> case execP (show n) of
        Left err -> Failed $ show n
        Right parsed -> show parsed === show n
    it "parses floats" do
      quickCheck \(n :: Number) -> case execP (show n) of
        Left err -> Failed $ show n
        Right parsed -> show parsed === show n
    it "manual tests" do
      (show <$> execP "1") `shouldEqual` (Right "1")
      runParser "1.78383e-3" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "1.78383e-3")
      runParser "0b101010101010101010101010101010101010101010101010101010101010101010" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "49191317529892137642")
      runParser "0B101010101010101010101010101010101010101010101010101010101010101010" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "49191317529892137642")
      runParser "0x1234567890abcdef1234567890abcdef" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "24197857200151252728969465429440056815")
      runParser "0X1234567890abcdef1234567890abcdef" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "24197857200151252728969465429440056815")
      runParser "0o56123423423423523452345234512341256245365236462452345642" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "269870138443162641462903354048649622586931538414498")
      runParser "0O56123423423423523452345234512341256245365236462452345642" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "269870138443162641462903354048649622586931538414498")

execP :: String -> Either ParseError ParsedExpr
execP input = do
  tokenStream <- runParser input tokenStreamParser
  runParser tokenStream tokenExprParser
