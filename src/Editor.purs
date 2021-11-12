module Editor where

import Prelude
import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Expression (prettyValue)
import Interpreter (Interpreter, eval, runInterpreter)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Text.Parsing.Parser (parseErrorMessage, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (TokenType(..), removeWhitespaceAndComments)
import Tokenizer as Tokenizer
import Unsafe.Coerce (unsafeCoerce)
import Utils (undefined, undefinedLog)

type EditorResult
  = { highlights :: Array (Array JSX)
    , results :: Array Answer
    }

data Answer
  = EmptyLine
  | SuccessResult String
  | ErrorResult String

run :: Array String -> EditorResult
run inputLines =
  let
    tokenize input = case Tokenizer.tokenize input of
      Left err -> undefinedLog $ "This happens when tokenization fails (should never fail): " <> show err
      Right t -> t

    tokens = tokenize <$> inputLines

    -- | Rather than short circuiting when we have a failed expression, we keep executing lines.
    alwaysSucceed :: Interpreter String -> Interpreter String
    alwaysSucceed couldFail =
      StateT
        ( \s -> case runStateT couldFail s of
            Left err -> Right (Tuple err s)
            success -> success
        )

    -- | Finds the first UnknownTk in the line if it exists.
    findUnknown :: Array TokenType -> Maybe TokenType
    findUnknown items = Array.head $ Array.filter isUnknown items
      where
      isUnknown (UnknownTk _) = true

      isUnknown _ = false

    -- | Evaluates the line, yielding the String result or error, or Nothing if the line is empty.
    evalLine :: Array TokenType -> Interpreter Answer
    evalLine [] = pure EmptyLine

    evalLine line
      | Maybe.isJust $ findUnknown line = case findUnknown line of
        Just tk -> pure $ ErrorResult $ "Did not understand \"" <> show tk <> "\""
        _ -> undefined
      | otherwise = case runParser line (tokenExprParser <* eof) of
        Left err -> pure $ ErrorResult $ parseErrorMessage err
        Right expr -> SuccessResult <$> (alwaysSucceed $ (prettyValue) <$> eval expr)

    -- | Convert the answer into the JS object.
    answerToJS (EmptyLine) = unsafeCoerce $ { empty: true }

    answerToJS (SuccessResult success) = unsafeCoerce $ { success }

    answerToJS (ErrorResult error) = unsafeCoerce $ { error }

    results :: Interpreter (Array Answer)
    results = traverse evalLine' $ removeWhitespaceAndComments <$> tokens
      where
      evalLine' t = answerToJS <$> evalLine t
  in
    { highlights: highlightTokens <$> tokens
    , results:
        case runInterpreter results of
          Left s -> undefinedLog $ "Interpreter failed: " <> (show s)
          Right r -> r
    }

highlightTokens :: Array TokenType -> Array JSX
highlightTokens input =
  let
    span :: String -> String -> JSX
    span className text = DOM.span { className, children: [ DOM.text text ] }

    convertToken :: TokenType -> JSX
    convertToken (NewlineTk) = undefinedLog "Cannot render newlines"

    convertToken (WhitespaceTk w) = span "whitespace" w

    convertToken p@(PunctuationTk _) = span "punctuation" (show p)

    convertToken (BaseLiteralTk n) = span "number" n

    convertToken (NumberTk n) = span "number" n

    convertToken (InfixTk i) = span "infix" i

    convertToken w@(ReservedTk _) = span "reserved" (show w)

    convertToken (NameTk n) = span "name" n

    convertToken (CommentTk c) = span "comment" c

    convertToken (UnknownTk c) = span "unknown" c
  in
    convertToken <$> input
