module Test.Utils where

import Prelude
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Utils as Utils

spec :: Spec Unit
spec = do
  describe "nonrepeatedCombinations" do
    it "generations combinations properly" do
      Utils.nonrepeatedCombinations ([] :: Array Int) `shouldEqual` []
      Utils.nonrepeatedCombinations ([ 1 ]) `shouldEqual` []
      Utils.nonrepeatedCombinations ([ 1, 2 ]) `shouldEqual` [ Tuple 1 2 ]
      Utils.nonrepeatedCombinations ([ 1, 2, 3 ]) `shouldEqual` [ Tuple 1 2, Tuple 1 3, Tuple 2 3 ]
