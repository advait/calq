module Utils where

import Prelude
import Data.BigNumber (BigNumber)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

-- | Format `BigNumber with a fixed number of decial places`
foreign import bigNumberFormatFixed :: Int -> BigNumber -> String

-- | Provide a fixed number of decimal places for `BigNumber`
foreign import bigNumberFixed :: Int -> BigNumber -> BigNumber

-- | Rounds the number to the nearest 0-2 decimal places.
bigNumberPretty :: BigNumber -> String
bigNumberPretty n
  -- TODO(advait): max decimal places should be configurable
  | n - (bigNumberFixed 0 n) < bigNum ".01" = bigNumberFormatFixed 0 n
  | n - (bigNumberFixed 1 n) < bigNum ".01" = bigNumberFormatFixed 1 n
  | otherwise = bigNumberFormatFixed 2 n

foreign import debugLog :: forall a. a -> a

foreign import debugLogAlt :: forall a b. a -> b -> b
