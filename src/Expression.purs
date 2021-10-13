module Expression where

import Prelude
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.NonEmpty ((:|))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Utils (parseBigNumber)

-- | Represents an expression that can be evaluated.
data Expr
  = Scalar BigNumber
  | Name String
  | Fn1 { name :: String, p1 :: Expr }
  | Fn2 { name :: String, p1 :: Expr, p2 :: Expr }
  | BindPrefix { name :: String, expr :: Expr }
  | BindRootUnit { name :: String }
  | BindUnit { name :: String, expr :: Expr }
  | BindAlias { name :: String, target :: String }
  | BindVariable { name :: String, expr :: Expr }

derive instance eqExpr :: Eq Expr

infixNames :: Array String
infixNames = [ "+", "-", "*", "/", "^" ]

instance showExpr :: Show Expr where
  show (Scalar n) = show n
  show (Name n) = n
  show (Fn1 { name, p1 }) = name <> "(" <> show p1 <> ")"
  show (Fn2 { name, p1, p2 })
    | Array.elem name infixNames = showWrapped p1 <> name <> showWrapped p2
    | otherwise = name <> "(" <> show p1 <> "," <> show p2 <> ")"
  show (BindPrefix { name, expr }) = "prefix " <> name <> " = " <> show expr
  show (BindRootUnit { name }) = "unit " <> name
  show (BindUnit { name, expr }) = "unit " <> name <> " = " <> show expr
  show (BindAlias { name, target }) = "alias " <> name <> " = " <> target
  show (BindVariable { name, expr }) = name <> " = " <> show expr

-- | Wraps the expression in parentheses if it's a compound expression.
showWrapped :: Expr -> String
showWrapped (Scalar n) = show n

showWrapped (Name n) = n

showWrapped e = "(" <> show e <> ")"

-- | TODO(advait): See if we can move Arbitrary to the test package.
instance arbitraryExpr :: Arbitrary Expr where
  arbitrary = genExpr 3

genBigNumber :: Gen BigNumber
genBigNumber = parseBigNumber <$> show <$> (arbitrary :: Gen Int)

genScalar :: Gen Expr
genScalar = Scalar <$> genBigNumber

genName :: Gen Expr
genName = Name <$> Gen.elements ("ft" :| [ "pi", "m", "advait" ])

genConcrete :: Gen Expr
genConcrete = Gen.oneOf (genScalar :| [ genName ])

genInfix :: Int -> Gen Expr
genInfix maxDepth
  | maxDepth <= 0 = genConcrete
  | otherwise = do
    name <- Gen.elements ("*" :| infixNames)
    p1 <- genExpr (maxDepth - 1)
    p2 <- genExpr (maxDepth - 1)
    pure $ Fn2 { name, p1, p2 }

genExpr :: Int -> Gen Expr
genExpr maxDepth
  | maxDepth <= 0 = genConcrete
  | otherwise = Gen.oneOf (genInfix maxDepth :| [])
