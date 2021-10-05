module TestUtils where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Effect.Exception (Error, error)
import Text.Parsing.Parser (Parser, runParser)

-- | Runs the given parser or fails the test.
runParserOrFail ::
  forall s a m.
  Show s =>
  MonadThrow Error m =>
  s -> Parser s a -> m a
runParserOrFail input p = case runParser input p of
  Right res -> pure res
  Left err -> throwError $ error ("Failed to parse " <> show input <> ": " <> show err)
