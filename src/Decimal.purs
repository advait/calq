module Decimal where

import Prelude hiding (mod)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int as Int
import Data.Ord (abs)
import Effect.Exception (Error)

foreign import data Decimal :: Type

instance showDecimal :: Show Decimal where
  show d = toString d

foreign import parseDecimalImpl :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error Decimal)

parseDecimal :: String -> Either Error Decimal
parseDecimal = runFn3 parseDecimalImpl Left Right

foreign import parseDecimalUnsafe :: String -> Decimal

fromInt :: Int -> Decimal
fromInt = parseDecimalUnsafe <<< show

foreign import toNumber :: Decimal -> Number

foreign import toString :: Decimal -> String

foreign import greaterThan :: Decimal -> Decimal -> Boolean

foreign import equals :: Decimal -> Decimal -> Boolean

instance eqDecimal :: Eq Decimal where
  eq d e = equals d e

instance ordDecimal :: Ord Decimal where
  compare d e =
    if greaterThan d e then
      GT
    else if equals d e then
      EQ
    else
      LT

foreign import plus :: Decimal -> Decimal -> Decimal

foreign import minus :: Decimal -> Decimal -> Decimal

foreign import times :: Decimal -> Decimal -> Decimal

foreign import dividedBy :: Decimal -> Decimal -> Decimal

instance semiringDecimal :: Semiring Decimal where
  add = plus
  zero = parseDecimalUnsafe "0"
  mul = times
  one = parseDecimalUnsafe "1"

instance ringDecimal :: Ring Decimal where
  sub = minus

isNegative :: Decimal -> Boolean
isNegative d = d < zero

foreign import floor :: Decimal -> Decimal

foreign import pow :: Decimal -> Decimal -> Decimal

-- | Returns whether this is a valid power (computationally possible).
canPow :: Decimal -> Boolean
canPow d = (abs d) <= (parseDecimalUnsafe "100")

powInt :: Decimal -> Int -> Decimal
powInt d e = pow d $ fromInt e

foreign import mod :: Decimal -> Decimal -> Decimal

instance commutativeRing :: CommutativeRing Decimal

instance divisionRing :: DivisionRing Decimal where
  recip d = one / d

instance euclideanRingDecimal :: EuclideanRing Decimal where
  degree = Int.floor <<< toNumber <<< floor <<< abs
  div = dividedBy
  mod = mod

foreign import toDecimalPlaces :: Int -> Decimal -> Decimal

pointZeroOne :: Decimal
pointZeroOne = parseDecimalUnsafe "0.01"
