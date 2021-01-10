module Test.Exponentials where

import Prelude
import Data.Group as Group
import Exponentials as Exponentials
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
