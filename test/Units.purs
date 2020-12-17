module Test.Units where

import Prelude
import Units
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.SortedArray as SortedArray
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "convertBaseUnit" do
    it "Converts m to m with 1.0" do
      (convertBaseUnit (Distance Meters) (Distance Meters)) `shouldEqual` (Just 1.0)
    it "Converts m to feet" do
      (convertBaseUnit (Distance Meters) (Distance Feet)) `shouldEqual` (Just 3.28084)
    it "Converts feet to m" do
      (convertBaseUnit (Distance Feet) (Distance Meters)) `shouldEqual` (Just 0.3047999902464003)
    it "Fails to convert length to time" do
      (convertBaseUnit (Distance Meters) (Time Seconds)) `shouldEqual` Nothing
  describe "convertCompositeUnit" do
    let
      scalarCompUnit = CompUnit { num: SortedArray.sort [], den: SortedArray.sort [] }
    let
      mps = CompUnit { num: SortedArray.sort [ Distance Meters ], den: SortedArray.sort [ Time Seconds ] }
    let
      fph = CompUnit { num: SortedArray.sort [ Distance Feet ], den: SortedArray.sort [ Time Hours ] }
    it "Converts scalars with 1.0" do
      convertCompUnit scalarCompUnit scalarCompUnit `shouldEqual` (Just 1.0)
    it "Converts mps to fph" do
      convertCompUnit mps fph `shouldApproxEqual` (Just 11811.014551188358)
    let
      m2 = CompUnit { num: SortedArray.sort [ Distance Meters, Distance Meters ], den: SortedArray.sort [] }
    let
      ft2 = CompUnit { num: SortedArray.sort [ Distance Feet, Distance Feet ], den: SortedArray.sort [] }
    it "Converts m^2 to f^2" do
      convertCompUnit m2 ft2 `shouldApproxEqual` (Just 10.7639)
    let
      s2 = CompUnit { num: SortedArray.sort [], den: SortedArray.sort [ Time Seconds, Time Seconds ] }

      hr2 = CompUnit { num: SortedArray.sort [], den: SortedArray.sort [ Time Hours, Time Hours ] }
    it "Converts 1/s^2 to 1/hr^2" do
      convertCompUnit s2 hr2 `shouldApproxEqual` (Just 12960000.0)

-- | Succeeds when the quantities are within .001% of each other.
shouldApproxEqual ::
  forall m.
  MonadThrow Error m =>
  Maybe Number -> Maybe Number -> m Unit
shouldApproxEqual Nothing Nothing = pure unit

shouldApproxEqual (Just v1) (Just v2) =
  when (abs (v1 - v2) / v1 > 1e-5)
    $ fail
    $ show v1
    <> " ~≠ "
    <> show v2

shouldApproxEqual v1 v2 = fail $ show v1 <> " ~≠ " <> show v2
