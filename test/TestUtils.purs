module TestUtils where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Test.QuickCheck (class Arbitrary, Result(..), (===))
import Test.Spec.QuickCheck (quickCheck)
import Parsing (Parser, runParser)

-- | Runs the given parser or fails the test.
runParserOrFail ::
  forall s a m.
  Show s =>
  MonadThrow Error m =>
  s -> Parser s a -> m a
runParserOrFail input p = case runParser input p of
  Right res -> pure res
  Left err -> throwError $ error ("Failed to parse " <> show input <> ": " <> show err)

-- | Helper to quickcheck the given parser.
quickCheckParser :: forall p. Arbitrary p => (p -> String) -> Parser String String -> Aff Unit
quickCheckParser genInput parser =
  quickCheck
    ( \(p :: p) ->
        let
          input = genInput p
        in
          case runParser input parser of
            Left err -> Failed $ show err
            Right parsed -> parsed === input
    )
