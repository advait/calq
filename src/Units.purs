module Units where

import Prelude
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.Tuple (Tuple(..))

data DistanceUnit
  = Meters
  | Feet

derive instance eqDistanceUnit :: Eq DistanceUnit

derive instance ordDistanceUnit :: Ord DistanceUnit

instance showDistanceUnit :: Show DistanceUnit where
  show Meters = "m"
  show Feet = "ft"

data TimeUnit
  = Seconds
  | Hours

derive instance eqTimeUnit :: Eq TimeUnit

derive instance ordTimeUnit :: Ord TimeUnit

instance showTimeUnit :: Show TimeUnit where
  show Seconds = "s"
  show Hours = "h"

-- | Represents a simple base unit with one dimension.
data BaseUnit
  = Distance DistanceUnit
  | Time TimeUnit

derive instance eqBaseUnit :: Eq BaseUnit

derive instance ordBaseUnit :: Ord BaseUnit

instance showBaseUnit :: Show BaseUnit where
  show (Distance d) = show d
  show (Time t) = show t

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
      , Tuple { from: Time Hours, to: Time Seconds } 3600.0
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

-- | A `CompUnit` is a `Semigroup` where we simply merge the numerators and merge the denominators.
instance semigroupCompUnit :: Semigroup CompUnit where
  append :: CompUnit -> CompUnit -> CompUnit
  append (CompUnit { num: n1, den: d1 }) (CompUnit { num: n2, den: d2 }) = CompUnit { num: n1 <> n2, den: d1 <> d2 }

-- | An empty `CompUnit` represents no units (i.e. a scalar quantity).
instance monoidCompUnit :: Monoid CompUnit where
  mempty = CompUnit { num: SortedArray.sort [], den: SortedArray.sort [] }

-- | A `CompUnit` with no numerator or denominator indicating a scalar quantity.
scalarCompUnit :: CompUnit
scalarCompUnit = CompUnit { num: SortedArray.sort [], den: SortedArray.sort [] }

-- | Given a number with the first unit, return what you need to multiply that number by to
-- | produce a quantity in the second unit.
convertCompUnit :: CompUnit -> CompUnit -> Maybe Number
convertCompUnit (CompUnit { num: n1, den: d1 }) (CompUnit { num: n2, den: d2 }) =
  let
    convertList :: List BaseUnit -> List BaseUnit -> Maybe Number
    convertList Nil Nil = Just 1.0

    convertList (Cons from tail1) (Cons to tail2) = (*) <$> (convertBaseUnit from to) <*> (convertList tail1 tail2)

    convertList _ _ = Nothing
  in
    do
      numeratorRatio <- convertList (List.fromFoldable n1) (List.fromFoldable n2)
      denominatorRatio <- convertList (List.fromFoldable d1) (List.fromFoldable d2)
      pure $ numeratorRatio / denominatorRatio

instance showCompUnit :: Show CompUnit where
  show _ = "[CompUnit/TODO]"

-- | Represents a Dimenseioned value - a number and a corresponding unit.
newtype DValue
  = DValue { scalar :: Number, unit :: CompUnit }
