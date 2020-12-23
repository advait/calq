module Utils where

import Prelude
import Data.BigNumber (BigNumber)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic
import Data.Tuple (Tuple(..))

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

pairwise :: forall a b. Array a -> Array b -> Array (Tuple a b)
pairwise a b = do
  a' <- a
  b' <- b
  pure $ Tuple a' b'
