module TokenParser where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.State (gets, put)
import Data.Array as Array
import Data.Maybe (Maybe(..), optional)
import Data.Tuple (Tuple(..))
import Expression (ParsedExpr(..))
import Text.Parsing.Parser (ParseState(..), Parser, ParserT, fail)
import Text.Parsing.Parser.Combinators (choice, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Pos (Position(..))
import Tokenizer (Punctuation(..), ReservedWord(..), TokenStream, TokenType(..))
import Utils (parseBigNumber)

emptyPosition :: Position
emptyPosition = Position { column: 0, line: 0 }

tk :: forall m. Monad m => TokenType -> ParserT TokenStream m TokenType
tk token = do
  Tuple input pos <- gets \(ParseState i pos _) -> Tuple i pos
  case Array.uncons input of
    Just { head, tail }
      | head == token -> do
        put $ ParseState tail pos true
        pure token
      | otherwise -> fail $ "Expected '" <> show token <> "' but instead got '" <> show head <> "'"
    Nothing -> fail "Unexpected end"

chomp :: forall m a. Monad m => (TokenType -> ParserT TokenStream m a) -> ParserT TokenStream m a
chomp delegate = do
  Tuple input pos <- gets \(ParseState i pos _) -> Tuple i pos
  case Array.uncons input of
    Just { head, tail } -> do
      result <- delegate head
      put $ ParseState tail pos true
      pure result
    Nothing -> fail "Unexpected end"

eof :: forall m. Monad m => ParserT TokenStream m Unit
eof = do
  input <- gets \(ParseState i _ _) -> i
  case Array.uncons input of
    Nothing -> pure unit
    Just something -> fail $ "Expected end"

nameParser :: forall m. Monad m => ParserT TokenStream m String
nameParser = chomp name'
  where
  name' :: TokenType -> ParserT TokenStream m String
  name' (NameTk name) = pure $ name

  name' _ = fail "Expected Name"

tokenExprParser :: Parser TokenStream ParsedExpr
tokenExprParser =
  let
    parenParser :: Parser TokenStream ParsedExpr
    parenParser = do
      _ <- tk $ PunctuationTk OpenParen
      expr <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure expr

    fn2Parser :: Parser TokenStream ParsedExpr
    fn2Parser = do
      name <- nameParser
      _ <- tk $ PunctuationTk OpenParen
      p1 <- tokenExprParser
      _ <- tk $ PunctuationTk Comma
      p2 <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure $ Fn2 { name, p1, p2 }

    fn1Parser :: Parser TokenStream ParsedExpr
    fn1Parser = do
      name <- nameParser
      _ <- tk $ PunctuationTk OpenParen
      p1 <- tokenExprParser
      _ <- tk $ PunctuationTk CloseParen
      pure $ Fn1 { name, p1 }

    numberParser :: Parser TokenStream ParsedExpr
    numberParser =
      let
        num' :: TokenType -> Parser TokenStream String
        num' (NumberTk n) = pure n

        num' _ = fail "Expected number"
      in
        do
          n <- chomp num'
          pure $ Scalar $ parseBigNumber n

    bindParser :: Parser TokenStream ParsedExpr
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

    bindAliasParser :: Parser TokenStream ParsedExpr
    bindAliasParser = do
      prefix <- tk $ ReservedTk AliasTk
      name <- nameParser
      _ <- tk $ PunctuationTk Equals
      target <- nameParser
      pure $ BindAlias { name, target }

    bindRootUnitParser :: Parser TokenStream ParsedExpr
    bindRootUnitParser = do
      prefix <- tk $ ReservedTk UnitTk
      name <- nameParser
      pure $ BindRootUnit { name }

    exprParserBase :: Parser TokenStream ParsedExpr
    exprParserBase =
      choice $ try
        <$> [ parenParser
          , fn2Parser
          , fn1Parser
          , bindParser
          , bindAliasParser
          , bindRootUnitParser
          , Name <$> nameParser
          , numberParser
          ]

    buildFn2 :: TokenType -> ParsedExpr -> ParsedExpr -> ParsedExpr
    buildFn2 token p1 p2 = Fn2 { name: show token, p1, p2 }

    parser =
      buildExprParser
        [ [ Infix (buildFn2 <$> (tk $ InfixTk "^")) AssocLeft
          ]
        , [ Infix (buildFn2 <$> (tk $ InfixTk "*")) AssocLeft
          , Infix (buildFn2 <$> (tk $ InfixTk "/")) AssocLeft
          -- | Implicit multiplication
          , Infix ((\p1 p2 -> Fn2 { name: "*", p1, p2 }) <$ pure unit) AssocLeft
          ]
        , [ Infix (buildFn2 <$> (tk $ InfixTk "+")) AssocLeft
          , Infix (buildFn2 <$> (tk $ InfixTk "-")) AssocLeft
          ]
        , [ Infix (buildFn2 <$> (tk $ ReservedTk InTk)) AssocLeft ]
        ]
        exprParserBase
  in
    parser <* eof
