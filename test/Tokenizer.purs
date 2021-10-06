module Test.Parsing where

import Prelude
import Data.Either (Either(..))
import Data.Number.Format as Format
import Parsing as Parsing
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (runParser)

spec :: Spec Unit
spec = do
  describe "Parsing" do
    it "parses integers" do
      quickCheck \(n :: Int) -> case runParser (show n) Parsing.floatParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)
    it "parses floats" do
      quickCheck \(n :: Number) -> case runParser (show n) Parsing.floatParser of
        Left err -> Failed $ show err
        Right parsed -> parsed === (show n)
    it "parses bignums" do
      quickCheck \(n :: Number) -> case runParser (show n) Parsing.bigNumParser of
        Left err -> Failed $ show err
        Right parsed -> (show parsed) === (show n)
    it "manual tests" do
      runParser "1.78383e-3" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "1.78383e-3")
      runParser "0b101010101010101010101010101010101010101010101010101010101010101010" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "49191317529892137642")
      runParser "0B101010101010101010101010101010101010101010101010101010101010101010" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "49191317529892137642")
      runParser "0x1234567890abcdef1234567890abcdef" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "24197857200151252728969465429440056815")
      runParser "0X1234567890abcdef1234567890abcdef" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "24197857200151252728969465429440056815")
      runParser "0o56123423423423523452345234512341256245365236462452345642" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "269870138443162641462903354048649622586931538414498")
      runParser "0O56123423423423523452345234512341256245365236462452345642" Parsing.bigNumParser `shouldEqual` (Right $ Parsing.bigNum "269870138443162641462903354048649622586931538414498")
