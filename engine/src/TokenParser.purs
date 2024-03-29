module TokenParser
  ( eof
  , matchToken
  , nameParser
  , returnP
  , tk
  , tokenExprParser
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..), optional)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Decimal (parseDecimalUnsafe)
import Decimal as Decimal
import Expression (Expr(..))
import Parsing (ParseState(..), Parser, ParserT, Position, fail, getParserT, stateParserT)
import Parsing.Combinators (choice, try)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Tokenizer (Punctuation(..), ReservedWord(..), TokenStream, TokenType(..))

-- | Manually set the given return value, stream, and position.
returnP :: forall m. Monad m => TokenType -> TokenStream -> Position -> ParserT TokenStream m TokenType
returnP tok stream pos = stateParserT (\_ -> Tuple tok $ ParseState stream pos true)

tk :: forall m. Monad m => TokenType -> ParserT TokenStream m TokenType
tk token = do
  ParseState input pos _ <- getParserT
  case Array.uncons input of
    Just { head, tail }
      | head == token -> returnP token tail pos
      | otherwise -> fail $ "Expected '" <> show token <> "' but instead got '" <> show head <> "'"
    Nothing -> fail "Unexpected end"

matchToken :: forall m. Monad m => (TokenType -> Boolean) -> ParserT TokenStream m TokenType
matchToken predicate = do
  ParseState input pos _ <- getParserT
  case Array.uncons input of
    Just { head, tail }
      | predicate head -> returnP head tail pos
      | otherwise -> fail $ "Did not expect \"" <> show head <> "\""
    Nothing -> fail "Unexpected end"

eof :: forall m. Monad m => ParserT TokenStream m Unit
eof = do
  ParseState input _ _ <- getParserT
  case Array.uncons input of
    Just _ -> fail $ "Expected end but got: " <> show input
    Nothing -> pure unit

nameParser :: forall m. Monad m => ParserT TokenStream m String
nameParser = show <$> matchToken name'
  where
  name' :: TokenType -> Boolean
  name' (NameTk _) = true

  name' _ = false

tokenExprParser :: Parser TokenStream Expr
tokenExprParser =
  let
    parenParser :: Parser TokenStream Expr
    parenParser = do
      _ <- tk $ PunctuationTk OpenParen
      expr <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure expr

    fn2Parser :: Parser TokenStream Expr
    fn2Parser = do
      name <- nameParser
      _ <- tk $ PunctuationTk OpenParen
      p1 <- tokenExprParser
      _ <- tk $ PunctuationTk Comma
      p2 <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure $ Fn2 { name, p1, p2 }

    fn1Parser :: Parser TokenStream Expr
    fn1Parser = do
      name <- nameParser
      _ <- tk $ PunctuationTk OpenParen
      p1 <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure $ Fn1 { name, p1 }

    numberParser :: Parser TokenStream Expr
    numberParser =
      let
        num' (NumberTk _) = true

        num' _ = false
      in
        do
          n <- show <$> matchToken num'
          let
            withoutCommas = replaceAll (Pattern ",") (Replacement "") n
          pure $ Scalar $ parseDecimalUnsafe withoutCommas

    bindParser :: Parser TokenStream Expr
    bindParser = do
      prefix <- optional $ ((tk $ ReservedTk UnitTk) <|> (tk $ ReservedTk PrefixTk))
      name <- nameParser
      _ <- tk $ PunctuationTk Equals
      expr <- tokenExprParser
      case prefix of
        Just (ReservedTk UnitTk) -> pure $ BindUnit { name, expr }
        Just (ReservedTk PrefixTk) -> pure $ BindPrefix { name, expr }
        Nothing -> pure $ BindVariable { name, expr }
        _ -> fail "Not a bind expression"

    bindAliasParser :: Parser TokenStream Expr
    bindAliasParser = do
      _ <- tk $ ReservedTk AliasTk
      name <- nameParser
      _ <- tk $ PunctuationTk Equals
      target <- nameParser
      pure $ BindAlias { name, target }

    bindRootUnitParser :: Parser TokenStream Expr
    bindRootUnitParser = do
      _ <- tk $ ReservedTk UnitTk
      name <- nameParser
      pure $ BindRootUnit { name }

    exprParserBase :: Parser TokenStream Expr
    exprParserBase =
      choice $ try
        <$>
          [ parenParser
          , fn2Parser
          , fn1Parser
          , bindParser
          , bindAliasParser
          , bindRootUnitParser
          , Name <$> nameParser
          , numberParser
          ]

    buildFn2 :: TokenType -> Expr -> Expr -> Expr
    buildFn2 token p1 p2 = Fn2 { name: show token, p1, p2 }
  in
    buildExprParser
      [ [ Infix (buildFn2 <$> (tk $ InfixTk "^")) AssocLeft
        ]
      , [ let
            -- If the second parameter is a negative scalar (e.g. 4-4),
            -- we should interpret as "4 - 4" instead of "4 * -4".
            buildImplicitMultiplication p1 (Scalar n)
              | Decimal.isNegative n = Fn2 { name: "-", p1, p2: Scalar $ negate n }

            -- If the second parameter is anything else, we should interpretet as implicit multiplication.
            buildImplicitMultiplication p1 p2 = Fn2 { name: "*", p1, p2 }
          in
            -- Note that implicit multiplication has higher priority than explicit multiplication
            -- or division so that "4m / 4m == 1".
            Infix (buildImplicitMultiplication <$ pure unit) AssocLeft
        ]
      , [ Infix (buildFn2 <$> (tk $ InfixTk "*")) AssocLeft
        , Infix (buildFn2 <$> (tk $ InfixTk "/")) AssocLeft
        ]
      , [ Infix (buildFn2 <$> (tk $ InfixTk "+")) AssocLeft
        , Infix (buildFn2 <$> (tk $ InfixTk "-")) AssocLeft
        ]
      , [ Infix (buildFn2 <$> (tk $ ReservedTk InTk)) AssocLeft ]
      ]
      exprParserBase
