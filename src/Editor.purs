module Editor where

import Prelude
import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Expression (prettyValue)
import Interpreter (Interpreter, eval, runInterpreter)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Text.Parsing.Parser (parseErrorMessage, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (TokenType(..), removeWhitespaceAndComments)
import Tokenizer as Tokenizer
import Utils (undefinedLog)

type EditorResult
  = { highlight :: Array JSX
    , results :: Array String
    }

run :: String -> EditorResult
run input =
  let
    tokens = case Tokenizer.tokenize input of
      Left err -> undefinedLog $ "This happens when tokenization fails (should never fail): " <> show err
      Right t -> t

    lines :: Array (Array TokenType)
    lines = removeWhitespaceAndComments <$> Tokenizer.lines tokens

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

    -- | Evaluates the line, yielding the String result or error.
    evalLine :: Array TokenType -> Interpreter String
    evalLine [] = pure $ ""

    evalLine line
      | Maybe.isJust $ findUnknown line = case findUnknown line of
        Just tk -> pure $ "Did not understand \"" <> show tk <> "\""
        _ -> undefined
      | otherwise = case runParser line (tokenExprParser <* eof) of
        Left err -> pure $ parseErrorMessage err
        Right expr -> alwaysSucceed $ (prettyValue) <$> eval expr

    results :: Interpreter (Array String)
    results = traverse evalLine lines
  in
    { highlight: highlightTokens tokens
    , results:
        case runInterpreter results of
          Left s -> [ "Interpreter failed: " <> (show s) ]
          Right r -> r
    }

highlightTokens :: Array TokenType -> Array JSX
highlightTokens input =
  let
    createSpan :: TokenType -> JSX
    createSpan (WhitespaceTk w) = DOM.span { className: "whitespace", children: [ DOM.text w ] }

    createSpan (NewlineTk) = DOM.br { className: "newline" }

    createSpan p@(PunctuationTk _) = DOM.span { className: "punctuation", children: [ DOM.text (show p) ] }

    createSpan (BaseLiteralTk n) = DOM.span { className: "number", children: [ DOM.text n ] }

    createSpan (NumberTk n) = DOM.span { className: "number", children: [ DOM.text n ] }

    createSpan (InfixTk i) = DOM.span { className: "infix", children: [ DOM.text i ] }

    createSpan w@(ReservedTk _) = DOM.span { className: "reserved", children: [ DOM.text (show w) ] }

    createSpan (NameTk n) = DOM.span { className: "name", children: [ DOM.text n ] }

    createSpan (CommentTk c) = DOM.span { className: "comment", children: [ DOM.text c ] }

    createSpan (UnknownTk c) = DOM.span { className: "unknown", children: [ DOM.text c ] }
  in
    -- Note that simple-react-code-editor requires a trailing <br> to function.
    (createSpan <$> input) <> [ DOM.br {} ]
