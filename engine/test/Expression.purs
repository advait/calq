module Test.Expression where

import Prelude
import Decimal (parseDecimalUnsafe)
import Expression (prettyNumber)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "prettyNumber" do
    let
      pretty = prettyNumber 2 <<< parseDecimalUnsafe
    it "drops decimals near an integer" do
      pretty "4" `shouldEqual` "4"
      pretty "4.004" `shouldEqual` "4"
      pretty "3.996" `shouldEqual` "4"
    it "keeps one decimal near a tenth" do
      pretty "6.5" `shouldEqual` "6.5"
      pretty "6.496" `shouldEqual` "6.5"
    it "rounds to two decimals otherwise" do
      pretty "0.9144" `shouldEqual` "0.91"
      pretty "13.125" `shouldEqual` "13.13"
      pretty "0.66" `shouldEqual` "0.66"
    it "handles negatives" do
      pretty "-0.9144" `shouldEqual` "-0.91"
      pretty "-4.004" `shouldEqual` "-4"
    it "honours other precisions" do
      let
        at places = prettyNumber places <<< parseDecimalUnsafe
      at 0 "0.9144" `shouldEqual` "1"
      at 0 "2.5" `shouldEqual` "3"
      at 4 "0.9144" `shouldEqual` "0.9144"
      at 4 "3.14159265" `shouldEqual` "3.1416"
      at 4 "2.00001" `shouldEqual` "2"
      at 10 "0.1" `shouldEqual` "0.1"
      at (-1) "1.6" `shouldEqual` "1.6"
      at (-1) "1e-21" `shouldEqual` "1e-21"
      at (-1) "1.4285714285714285714e-11" `shouldEqual` "1.4285714285714285714e-11"
