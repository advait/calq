module Main where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.SortedArray (SortedArray)
import Data.Tuple (Tuple(..))

data DistanceUnit
  = Meters
  | Feet

derive instance eqDistanceUnit :: Eq DistanceUnit

derive instance ordDistanceUnit :: Ord DistanceUnit

data TimeUnit
  = Seconds
  | Hours

derive instance eqTimeUnit :: Eq TimeUnit

derive instance ordTimeUnit :: Ord TimeUnit

-- | Represents a simple base unit with one dimension.
data BaseUnit
  = Distance DistanceUnit
  | Time TimeUnit

derive instance eqBaseUnit :: Eq BaseUnit

derive instance ordBaseUnit :: Ord BaseUnit

-- | Given a number with the first unit, return what you need to multiply that number by to
-- | produce a quantity in the second unit.
convertBaseUnit :: BaseUnit -> BaseUnit -> Maybe Number
convertBaseUnit a b
  | a == b = Just 1.0

convertBaseUnit a b =
  let
    ratios :: Array (Tuple { from :: BaseUnit, to :: BaseUnit } Number)
    ratios =
      [ Tuple { from: Distance Meters, to: Distance Feet } 3.28084
      , Tuple { from: Time Seconds, to: Time Hours } 0.000277778
      ]

    inverses :: Array (Tuple { from :: BaseUnit, to :: BaseUnit } Number)
    inverses = (\(Tuple { from, to } ratio) -> Tuple { from: to, to: from } (1.0 / ratio)) <$> ratios

    -- TODO(advait): Support acyclic graphs of conversions without duplicatively representing ratios
    -- Either consider using a search algorithm against this graph or pre-computing all possible
    -- traversals upfront.
    mappings :: Map.Map { from :: BaseUnit, to :: BaseUnit } Number
    mappings = Map.fromFoldable (ratios <> inverses)
  in
    Map.lookup { from: a, to: b } mappings

-- | Represents a complex unit composed of product/quotient of multiple `BaseUnit`s.
newtype CompUnit
  = CompUnit
  { num :: SortedArray BaseUnit
  , den :: SortedArray BaseUnit
  }

derive instance eqCompUnit :: Eq CompUnit

-- | Represents a Dimenseioned value - a number and a corresponding unit.
newtype DValue
  = DValue { scalar :: Number, unit :: CompUnit }
