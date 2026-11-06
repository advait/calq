module Editor where

import Prelude
import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Expression (prettyValueWith)
import Interpreter (Interpreter, eval, runInterpreter)
import Parsing (parseErrorMessage, runParser)
import TokenParser (eof, tokenExprParser)
import Tokenizer (TokenType(..), removeWhitespaceAndComments)
import Tokenizer as Tokenizer
import Unsafe.Coerce (unsafeCoerce)
import Utils (undefined_, undefinedLog)

type EditorResult =
  { highlights :: Array (Array Highlight)
  , results :: Array Answer
  }

data Answer
  = EmptyLine
  | SuccessResult String
  | ErrorResult String

derive instance eqAnswer :: Eq Answer

instance showAnswer :: Show Answer where
  show EmptyLine = "EmptyLine"
  show (SuccessResult s) = "(SuccessResult " <> show s <> ")"
  show (ErrorResult s) = "(ErrorResult " <> show s <> ")"

-- | Entry point for the frontend: each line's highlights and its answer as a JS object, with
-- | answers shown to at most `places` decimal places (negative for every digit).
run :: Int -> Array String -> EditorResult
run places inputLines =
  let
    { highlights, answers } = evaluateWith places inputLines

    answerToJS (EmptyLine) = unsafeCoerce $ { empty: true }

    answerToJS (SuccessResult success) = unsafeCoerce $ { success }

    answerToJS (ErrorResult error) = unsafeCoerce $ { error }
  in
    { highlights, results: answerToJS <$> answers }

-- | Highlights and evaluates each line in order, showing answers to calq's default two decimal
-- | places; a failing line doesn't stop the lines after it.
evaluate :: Array String -> { highlights :: Array (Array Highlight), answers :: Array Answer }
evaluate = evaluateWith 2

evaluateWith :: Int -> Array String -> { highlights :: Array (Array Highlight), answers :: Array Answer }
evaluateWith places inputLines =
  let
    tokenize input = case Tokenizer.tokenize input of
      Left err -> undefinedLog $ "This happens when tokenization fails (should never fail): " <> show err
      Right t -> t

    tokens = tokenize <$> inputLines

    -- | Rather than short circuiting when a line fails, we report its error and keep executing
    -- | the remaining lines.
    recover :: Interpreter Answer -> Interpreter Answer
    recover couldFail =
      StateT
        ( \s -> case runStateT couldFail s of
            Left err -> Right (Tuple (ErrorResult err) s)
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
          _ -> undefined_
      | otherwise = case runParser line (tokenExprParser <* eof) of
          Left err -> pure $ ErrorResult $ parseErrorMessage err
          Right expr -> recover $ SuccessResult <<< prettyValueWith places <$> eval expr

    answers :: Interpreter (Array Answer)
    answers = traverse evalLine $ removeWhitespaceAndComments <$> tokens
  in
    { highlights: highlightTokens <$> tokens
    , answers:
        case runInterpreter answers of
          Left s -> undefinedLog $ "Interpreter failed: " <> (show s)
          Right r -> r
    }

type Highlight =
  { highlightType :: String
  , text :: String
  }

highlightTokens :: Array TokenType -> Array Highlight
highlightTokens input =
  let
    convertToken :: TokenType -> Highlight
    convertToken (NewlineTk) = undefinedLog "Cannot render newlines"

    convertToken (WhitespaceTk w) = { highlightType: "whitespace", text: w }

    convertToken p@(PunctuationTk _) = { highlightType: "punctuation", text: show p }

    convertToken (BaseLiteralTk n) = { highlightType: "number", text: n }

    convertToken (NumberTk n) = { highlightType: "number", text: n }

    convertToken (InfixTk i) = { highlightType: "infix", text: i }

    convertToken w@(ReservedTk _) = { highlightType: "reserved", text: show w }

    convertToken (NameTk n) = { highlightType: "name", text: n }

    convertToken (CommentTk c) = { highlightType: "comment", text: c }

    convertToken (UnknownTk c) = { highlightType: "unknown", text: c }
  in
    convertToken <$> input
