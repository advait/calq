module Editor where

import Prelude
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Expression (ParsedExpr)
import Interpreter (Interpreter, InterpreterState, eval, runInterpreter)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Text.Parsing.Parser (ParseError(..), runParser)
import TokenParser (tokenExprParser)
import Tokenizer (TokenStream, TokenType(..))
import Tokenizer as Tokenizer
import Utils (undefinedLog)

type EditorResult
  = { highlight :: Array ReactElement
    , results :: Array String
    }

run :: String -> EditorResult
run input =
  let
    tokens = case Tokenizer.tokenize input of
      Left err -> undefinedLog $ "This happens when tokenization fails (should never fail): " <> show err
      Right t -> t

    lines :: Array (Array TokenType)
    lines = Tokenizer.lines tokens

    parseExpr :: TokenStream -> Either ParseError ParsedExpr
    parseExpr line = runParser line tokenExprParser

    evalLine :: Array TokenType -> Interpreter String
    evalLine line = case parseExpr line of
      Left err -> pure $ show err
      Right expr -> show <$> eval expr

    results :: Interpreter (Array String)
    results = traverse evalLine lines
  in
    { highlight: highlightTokens tokens
    , results:
        case runInterpreter results of
          Left s -> undefinedLog $ "Interpreter failed: " <> (show s)
          Right r -> r
    }

highlightTokens :: Array TokenType -> Array ReactElement
highlightTokens input =
  let
    createSpan :: TokenType -> ReactElement
    createSpan (WhitespaceTk w) = DOM.span [ Props.className "whitespace" ] [ DOM.text w ]

    createSpan (NewlineTk) = DOM.br [ Props.className "newline" ]

    createSpan p@(PunctuationTk _) = DOM.span [ Props.className "punctuation" ] [ DOM.text (show p) ]

    createSpan (BaseLiteralTk n) = DOM.span [ Props.className "number" ] [ DOM.text n ]

    createSpan (NumberTk n) = DOM.span [ Props.className "number" ] [ DOM.text n ]

    createSpan (InfixTk i) = DOM.span [ Props.className "infix" ] [ DOM.text i ]

    createSpan w@(ReservedTk _) = DOM.span [ Props.className "reserved" ] [ DOM.text (show w) ]

    createSpan (NameTk n) = DOM.span [ Props.className "name" ] [ DOM.text n ]

    createSpan (CommentTk c) = DOM.span [ Props.className "comment" ] [ DOM.text c ]

    createSpan (UnknownTk c) = DOM.span [ Props.className "unknown" ] [ DOM.text c ]
  in
    -- Note that simple-react-code-editor requires a trailing <br> to function.
    (createSpan <$> input) <> [ DOM.br [] ]
