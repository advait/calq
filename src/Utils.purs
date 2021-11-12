module Utils where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Tuple (Tuple(..))

foreign import debugLog :: forall a. a -> a

foreign import undefined :: forall a. a

foreign import undefinedLog :: forall a. String -> a

-- | Contents of Definitions.calq.
foreign import definitionsFile :: String

-- | Contents of InterpreterTest.calq.
foreign import interpreterTestFile :: String

-- | Returns a array of all pair combinations between the two arrays.
combinationsWithIndex :: forall a b. Array a -> Array b -> Array (Tuple (Tuple Int a) (Tuple Int b))
combinationsWithIndex a b = do
  a' <- (mapWithIndex Tuple a)
  b' <- (mapWithIndex Tuple b)
  pure $ Tuple a' b'

-- | Given a single array, return all the non-repeated pair combinations of elements in the array.
nonrepeatedCombinations :: forall a. Array a -> Array (Tuple a a)
nonrepeatedCombinations arr =
  let
    stripIndex :: Tuple (Tuple Int a) (Tuple Int a) -> Tuple a a
    stripIndex (Tuple (Tuple _ a) (Tuple _ b)) = Tuple a b

    indexFn :: (Int -> Int -> Boolean) -> (Tuple (Tuple Int a) (Tuple Int a)) -> Boolean
    indexFn f (Tuple (Tuple iA _) (Tuple iB _)) = f iA iB

    nonrepeatedItems = Array.filter (indexFn (<)) $ combinationsWithIndex arr arr
  in
    stripIndex <$> nonrepeatedItems
