module Expression where

import Prelude
import Data.Array as Array
import Data.BigNumber (BigNumber)

-- | Represents an expression that can be evaluated.
data ParsedExpr
  = Scalar BigNumber
  | Name String
  | Fn1 { name :: String, p1 :: ParsedExpr }
  | Fn2 { name :: String, p1 :: ParsedExpr, p2 :: ParsedExpr }
  | BindPrefix { name :: String, expr :: ParsedExpr }
  | BindRootUnit { name :: String }
  | BindUnit { name :: String, expr :: ParsedExpr }
  | BindAlias { name :: String, target :: String }
  | BindVariable { name :: String, expr :: ParsedExpr }

derive instance eqParsedExpr :: Eq ParsedExpr

infixNames :: Array String
infixNames = [ "+", "-", "*", "/", "^" ]

instance showParsedExpr :: Show ParsedExpr where
  show (Scalar n) = show n
  show (Name n) = n
  show (Fn1 { name, p1 }) = name <> "(" <> show p1 <> ")"
  show (Fn2 { name, p1, p2 })
    | Array.elem name infixNames = show p1 <> name <> show p2
    | otherwise = name <> "(" <> show p1 <> "," <> show p2 <> ")"
  show (BindPrefix { name, expr }) = "prefix " <> name <> " = " <> show expr
  show (BindRootUnit { name }) = "unit " <> name
  show (BindUnit { name, expr }) = "unit " <> name <> " = " <> show expr
  show (BindAlias { name, target }) = "alias " <> name <> " = " <> target
  show (BindVariable { name, expr }) = name <> " = " <> show expr
