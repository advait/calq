module Test.Exponentials where

import Prelude
import Data.Group as Group
import Data.Tuple (Tuple(..))
import Exponentials as Exponentials
import Test.QuickCheck (assertGreaterThan)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Exponentials" do
    let
      a = Exponentials.singleton "A"
    it "Handles basic multiplication" do
      (a <> a) `shouldEqual` (Exponentials.power "A" 2)
    it "Cancels out inverses" do
      (a <> (Group.ginverse a)) `shouldEqual` mempty
    it "Handles power inverses" do
      ((Exponentials.power "A" 3) <> (Exponentials.power "A" (-3))) `shouldEqual` mempty
    it "Is able to print exponentials without crashing" do
      quickCheck \((Tuple n1 n2) :: Tuple Int Int) ->
        let
          exp = Exponentials.power "a" n1 <> Exponentials.power "b" n2
        in
          show exp `assertGreaterThan` ""
