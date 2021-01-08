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

foreign import debugLog :: forall a. a -> a

foreign import debugLogAlt :: forall a b. a -> b -> b

foreign import undefinedLog :: forall a. String -> a
