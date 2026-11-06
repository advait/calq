module Test.Editor where

import Prelude
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as String
import Editor (Answer(..), evaluate)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "evaluate" do
    let
      answers = _.answers <<< evaluate
    it "answers each line" do
      answers [ "1 ft in inches", "", "2 * 2" ]
        `shouldEqual` [ SuccessResult "12 inch", EmptyLine, SuccessResult "4" ]
    it "reports evaluation errors as errors" do
      answers [ "bogus + 1" ] `shouldEqual` [ ErrorResult "Undefined variable \"bogus\"" ]
    it "treats Unicode spaces as whitespace" do
      answers [ "1" <> "\xa0" <> "ft + 1" <> "\x3000" <> "ft in inches" ] `shouldEqual` [ SuccessResult "24 inch" ]
    it "keeps evaluating after a failed line" do
      answers [ "x = 2", "bogus", "x * 3" ]
        `shouldEqual` [ SuccessResult "2", ErrorResult "Undefined variable \"bogus\"", SuccessResult "6" ]
  describe "highlights" do
    -- The editor overlays highlights on the raw text, so they must reproduce it exactly.
    it "reproduce the source line" do
      quickCheck \s -> do
        let
          line = String.replaceAll (String.Pattern "\n") (String.Replacement " ") s
          rendered = foldMap (foldMap _.text) (evaluate [ line ]).highlights
        rendered === line
    it "reproduce typical lines" do
      let
        lines =
          [ "1 m/s * 3 years in km", "  x\t= 2e-3 μm  # note", "π * (2 ft)^2", "1,000.50 USD/hr", "IN In in" ]
      Array.fold (foldMap _.text <$> (evaluate lines).highlights) `shouldEqual` Array.fold lines
