module Utils where

import Prelude
import Data.BigNumber (BigNumber)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic
import Web.Event.Internal.Types (Event)

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

-- | Logs the item to the console and returns it.
foreign import debugLog :: forall a. a -> a

foreign import debugLogAlt :: forall a b. a -> b -> b

foreign import eventTargetTextContent :: Event -> String

foreign import refireEvent :: forall a. Event -> a -> a
