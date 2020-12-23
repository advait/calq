module Test.Units where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.BigNumber (BigNumber)
import Data.Either (Either(..))
import Data.Ord (abs)
import Data.SortedArray as SortedArray
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Units (BaseUnit(..), CompUnit(..), DistanceUnit(..), MassUnit(..), TimeUnit(..), convertBaseUnit, convertCompUnit)
import Utils (bigNum)

spec :: Spec Unit
spec = do
  describe "convertBaseUnit" do
    it "Converts m to m with 1.0" do
      convertBaseUnit (Distance Meters) (Distance Meters) `shouldApproxEqual` (Right $ bigNum "1.0")
    it "Converts m to feet" do
      convertBaseUnit (Distance Meters) (Distance Feet) `shouldApproxEqual` (Right $ bigNum "3.28084")
    it "Converts feet to m" do
      convertBaseUnit (Distance Feet) (Distance Meters) `shouldApproxEqual` (Right $ bigNum "0.3048")
    it "Fails to convert length to time" do
      convertBaseUnit (Distance Meters) (Time Seconds) `shouldApproxEqual` Left ""
    it "Converts km to ft (bfs)" do
      convertBaseUnit (Distance Kilometers) (Distance Feet) `shouldApproxEqual` (Right $ bigNum "3280.84")
    it "Converts tons to nanograms (bfs)" do
      convertBaseUnit (Mass Tons) (Mass Nanograms) `shouldApproxEqual` (Right $ bigNum "907184740000000")
  describe "convertCompositeUnit" do
    let
      scalarCompUnit = compUnit [] []
    it "Converts scalars with 1.0" do
      convertCompUnit scalarCompUnit scalarCompUnit `shouldEqual` (Right $ bigNum "1.0")
    let
      mps = compUnit [ Distance Meters ] [ Time Seconds ]

      fph = compUnit [ Distance Feet ] [ Time Hours ]
    it "Converts mps to fph" do
      convertCompUnit mps fph `shouldApproxEqual` (Right $ bigNum "11811")
    let
      m2 = compUnit [ Distance Meters, Distance Meters ] []

      ft2 = compUnit [ Distance Feet, Distance Feet ] []
    it "Converts m^2 to f^2" do
      convertCompUnit m2 ft2 `shouldApproxEqual` (Right $ bigNum "10.7639")
    let
      s2 = compUnit [] [ Time Seconds, Time Seconds ]

      hr2 = compUnit [] [ Time Hours, Time Hours ]
    it "Converts 1/s^2 to 1/hr^2" do
      convertCompUnit s2 hr2 `shouldApproxEqual` (Right $ bigNum "12960000.0")
    it "Fails to convert incompatible units" do
      convertCompUnit mps m2 `shouldApproxEqual` (Left "")
      convertCompUnit scalarCompUnit m2 `shouldApproxEqual` (Left "")

-- | Succeeds when the quantities are within .001% of each other.
shouldApproxEqual ::
  forall m.
  MonadThrow Error m =>
  Either String BigNumber -> Either String BigNumber -> m Unit
shouldApproxEqual (Left _) (Left _) = pure unit

shouldApproxEqual (Right v1) (Right v2) =
  when (abs (v1 - v2) / v2 > (bigNum "1e-5"))
    $ fail
    $ show v1
    <> " ~≠ "
    <> show v2

shouldApproxEqual v1 v2 = fail $ show v1 <> " ~≠ " <> show v2

compUnit :: Array BaseUnit -> Array BaseUnit -> CompUnit
compUnit num den = CompUnit { num: SortedArray.sort num, den: SortedArray.sort den }
