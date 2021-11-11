module Decimal where

import Prelude
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3, Fn2, runFn2, Fn5, runFn5)
import Data.Ord (class Ord)
import Effect.Exception (Error)

foreign import data Decimal :: Type

instance showDecimal :: Show Decimal where
  show d = toString d

foreign import parseDecimalImpl :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error Decimal)

parseDecimal :: String -> Either Error Decimal
parseDecimal = runFn3 parseDecimalImpl Left Right

foreign import parseDecimalUnsafe :: String -> Decimal

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

foreign import times :: Decimal -> Decimal -> Decimal

instance semiringDecimal :: Semiring Decimal where
  add = plus
  zero = parseDecimalUnsafe "0"
  mul = times
  one = parseDecimalUnsafe "1"

foreign import minus :: Decimal -> Decimal -> Decimal

instance ringDecimal :: Ring Decimal where
  sub = minus
