module Utils where

import Prelude
import Data.BigNumber (BigNumber)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument
