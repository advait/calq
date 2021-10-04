module Editor where

import Prelude
import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Interpreter (Interpreter, eval, runInterpreter)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Text.Parsing.Parser (runParser)
import TokenParser (tokenExprParser)
import Tokenizer (TokenType(..))
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
    lines = removeWhitespace <$> Tokenizer.lines tokens
      where
      removeWhitespace = Array.filter f

      f (WhitespaceTk _) = false

      f _ = true

    -- | Rather than short circuiting when we have a failed expression, we keep executing lines.
    alwaysSucceed :: Interpreter String -> Interpreter String
    alwaysSucceed couldFail =
      StateT
        ( \s -> case runStateT couldFail s of
            Left err -> Right (Tuple err s)
            success -> success
        )

    evalLine :: Array TokenType -> Interpreter String
    evalLine line = case runParser line tokenExprParser of
      Left err -> pure $ show err
      Right expr -> alwaysSucceed $ show <$> eval expr

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
