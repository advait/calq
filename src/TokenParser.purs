module TokenParser where

import Prelude
import Control.Monad.State (gets, modify_, put)
import Control.Monad.State.Trans as StateT
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression (ParsedExpr(..))
import Text.Parsing.Parser (ParseState(..), Parser, ParserT(..), fail)
import Text.Parsing.Parser.Combinators (choice, skipMany, try)
import Text.Parsing.Parser.Pos (Position(..))
import Tokenizer (Punctuation(..), TokenStream, TokenType(..), tokenStreamParser)
import Utils (parseBigNumber, undefinedLog)

incrPos :: Position -> Position
incrPos (Position { column, line }) = Position { column: column + 1, line }

tk :: forall m. Monad m => TokenType -> ParserT TokenStream m TokenType
tk token = do
  --   input :: TokenStream <- (lift $ gets \(ParseState input _ _) -> input) :: Parser TokenStream TokenStream
  input :: TokenStream <- gets \(ParseState i _ _) -> i
  case Array.uncons input of
    Just { head: Tuple foundTk pos, tail }
      | foundTk == token -> do
        put $ ParseState tail (incrPos pos) true
        pure token
      | otherwise -> fail $ "Expected '" <> show token <> "' but instead got '" <> show foundTk <> "'"
    Nothing -> fail "Unexpected end"

chomp :: forall m a. Monad m => (TokenType -> ParserT TokenStream m a) -> ParserT TokenStream m a
chomp delegate = do
  input :: TokenStream <- gets \(ParseState i _ _) -> i
  case Array.uncons input of
    Just { head: Tuple foundTk pos, tail } -> delegate foundTk
    Nothing -> fail "Unexpected end"

nameParser :: forall m. Monad m => ParserT TokenStream m String
nameParser = chomp name'
  where
  name' :: TokenType -> ParserT TokenStream m String
  name' (NameTk name) = pure $ name

  name' _ = fail "Expected Name"

whitespace :: forall m. Monad m => ParserT TokenStream m Unit
whitespace = chomp ws'
  where
  ws' :: TokenType -> ParserT TokenStream m Unit
  ws' (WhitespaceTk _) = pure unit

  ws' _ = fail "Expected whitespace"

lexeme :: forall m a. Monad m => ParserT TokenStream m a -> ParserT TokenStream m a
lexeme p = do
  ret <- p
  _ <- skipMany whitespace
  pure ret

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
      _ <- lexeme $ tk $ PunctuationTk OpenParen
      p1 <- lexeme $ tokenExprParser
      _ <- lexeme $ tk $ PunctuationTk Comma
      p2 <- lexeme $ tokenExprParser
      _ <- lexeme $ tk $ PunctuationTk CloseParen
      pure $ Fn2 { name, p1, p2 }

    fn1Parser :: Parser TokenStream ParsedExpr
    fn1Parser = do
      name <- nameParser
      _ <- lexeme $ tk $ PunctuationTk OpenParen
      p1 <- lexeme $ tokenExprParser
      _ <- lexeme $ tk $ PunctuationTk CloseParen
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
  -- bindPrefixParser :: Parser String ParsedExpr
  -- bindPrefixParser = do
  --   _ <- lexeme $ string "prefix"
  --   name <- nameParser
  --   _ <- lexeme $ string "="
  --   expr <- exprParser
  --   pure $ BindPrefix { name, expr }
  -- bindUnitParser :: Parser String ParsedExpr
  -- bindUnitParser = do
  --   _ <- lexeme $ string "unit"
  --   name <- nameParser
  --   _ <- lexeme $ string "="
  --   expr <- exprParser
  --   pure $ BindUnit { name, expr }
  -- bindRootUnitParser :: Parser String ParsedExpr
  -- bindRootUnitParser = do
  --   _ <- lexeme $ string "unit"
  --   name <- nameParser
  --   pure $ BindRootUnit { name }
  -- bindAliasParser :: Parser String ParsedExpr
  -- bindAliasParser = do
  --   _ <- lexeme $ string "alias"
  --   name <- nameParser
  --   _ <- lexeme $ string "="
  --   target <- nameParser
  --   pure $ BindAlias { name, target }
  -- bindVariableParser :: Parser String ParsedExpr
  -- bindVariableParser = do
  --   name <- nameParser
  --   _ <- lexeme $ string "="
  --   expr <- exprParser
  --   pure $ BindVariable { name, expr }
  -- scalarParser :: Parser String ParsedExpr
  -- scalarParser = do
  --   value <- bigNumParser
  --   pure $ Scalar value
  -- nameParser :: Parser String String
  -- nameParser =
  --   let
  --     name =
  --       lexeme
  --         $ do
  --             head <- satisfy startingChar
  --             tail <- Array.many $ satisfy bodyChar
  --             pure $ fromCharArray $ Array.cons head tail
  --     startingChar c = isAlpha c || c `elem` [ '\'', '"' ]
  --     bodyChar c = isAlphaNum c || c `elem` [ '\'', '"' ]
  --     notReserved :: String -> Parser String String
  --     notReserved s =
  --       if elem s [ "in" ] then
  --         fail $ "reserved word " <> s
  --       else
  --         pure s
  --   in
  --     name >>= notReserved
  -- exprParserBase =
  --   choice $ try
  --     <$> [ parenParser
  --       , fn2Parser
  --       , fn1Parser
  --       , bindPrefixParser
  --       , bindUnitParser
  --       , bindRootUnitParser
  --       , bindAliasParser
  --       , bindVariableParser
  --       , scalarParser
  --       , Name <$> nameParser
  --       ]
  -- buildFn2 :: String -> ParsedExpr -> ParsedExpr -> ParsedExpr
  -- buildFn2 name p1 p2 = Fn2 { name, p1, p2 }
  -- exprParserExponents =
  --   buildExprParser
  --     [ [ Infix (buildFn2 <$> (lexeme $ string "^")) AssocLeft ] ]
  --     (lexeme exprParserBase)
  -- exprParserImplicitTimes :: Parser String ParsedExpr
  -- exprParserImplicitTimes = do
  --   exprs <- NonEmptyArray.some exprParserExponents
  --   pure $ foldl1 (buildFn2 "*") $ NonEmptyArray.toNonEmpty exprs
  in
    choice $ try <$> [ parenParser, fn2Parser, fn1Parser, numberParser ]

-- do
-- _ <- spaces
-- buildExprParser
-- [ [ Infix (buildFn2 <$> (lexeme $ string "*")) AssocLeft
-- , Infix (buildFn2 <$> (lexeme $ string "/")) AssocLeft
-- ]
-- , [ Infix (buildFn2 <$> (lexeme $ string "+")) AssocLeft
-- , Infix (buildFn2 <$> (lexeme $ string "-")) AssocLeft
-- ]
-- , [ Infix (buildFn2 <$> (lexeme $ string "in")) AssocLeft ]
-- ]
-- (lexeme exprParserImplicitTimes)
