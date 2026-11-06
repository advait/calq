module Test.Expression where

import Prelude
import Decimal (parseDecimalUnsafe)
import Expression (prettyBigNum)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "prettyBigNum" do
    let
      pretty = prettyBigNum <<< parseDecimalUnsafe
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
