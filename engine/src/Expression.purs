module Expression where

import Prelude
import Data.Array (unsafeIndex)
import Data.Array as Array
import Decimal (Decimal, toDecimalPlaces, fromInt, pointZeroOne)
import Exponentials (Exponentials)
import Exponentials as Exponentials
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

-- | Represents an expression that can be evaluated.
data Expr
  = Scalar Decimal
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

-- | Represents a concrete unit like "m" (meters).
type ConcreteUnit = String

-- | Our interpreter evaluates expressions into these values.
type Value = { scalar :: Decimal, units :: Exponentials ConcreteUnit }

scalar1 :: Value
scalar1 = { scalar: one, units: mempty }

prettyBigNum :: Decimal -> String
prettyBigNum n
  | n - (toDecimalPlaces 0 n) < pointZeroOne = show $ toDecimalPlaces 0 n
  | n - (toDecimalPlaces 1 n) < pointZeroOne = show $ toDecimalPlaces 1 n
  | otherwise = show $ toDecimalPlaces 2 n

prettyValue :: Value -> String
prettyValue { scalar, units }
  | units == mempty = prettyBigNum scalar
  | otherwise = prettyBigNum scalar <> " " <> show units

singletonUnit :: ConcreteUnit -> Value
singletonUnit unit = { scalar: one, units: Exponentials.singleton unit }

-- | TODO(advait): See if we can move Arbitrary to the test package.
instance arbitraryExpr :: Arbitrary Expr where
  arbitrary = genExpr 3

genDecimal :: Gen Decimal
genDecimal = fromInt <$> (arbitrary :: Gen Int)

genScalar :: Gen Expr
genScalar = Scalar <$> genDecimal

genSelect :: forall a. Array a -> Gen a
genSelect choices = do
  n <- chooseInt zero (Array.length choices - 1)
  pure $ unsafePartial $ unsafeIndex choices n

genOneOf :: forall a. Array (Gen a) -> Gen a
genOneOf choices = do
  n <- chooseInt zero (Array.length choices - 1)
  unsafePartial $ unsafeIndex choices n

genName :: Gen Expr
genName = Name <$> genSelect [ "ft", "pi", "m", "advait" ]

-- | A concrete value is a non-recursive value.
genConcrete :: Gen Expr
genConcrete = genOneOf [ genScalar, genName ]

genInfix :: Int -> Gen Expr
genInfix maxDepth
  | maxDepth <= 0 = genConcrete
  | otherwise = do
      name <- genSelect $ [ "*" ] <> infixNames
      p1 <- genExpr (maxDepth - 1)
      p2 <- genExpr (maxDepth - 1)
      pure $ Fn2 { name, p1, p2 }

-- | Because expressions are recursive data structures (4 + (3 * 5)), we need to limit their
-- | depth while generating them otherwise they can be infinitely large.
genExpr :: Int -> Gen Expr
genExpr maxDepth
  | maxDepth <= 0 = genConcrete
  | otherwise = genOneOf [ genInfix maxDepth ]
