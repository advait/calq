module Units where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.BigNumber (BigNumber)
import Data.DivisionRing as DivisionRing
import Data.Foldable (class Foldable)
import Data.Generic.Rep (Argument(..), Constructor(..))
import Data.Generic.Rep as Generic
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.SortedArray (SortedArray)
import Data.SortedArray as SortedArray
import Data.Tuple (Tuple(..))

data DistanceUnit
  = Meters
  | Kilometers
  | Feet

derive instance eqDistanceUnit :: Eq DistanceUnit

derive instance ordDistanceUnit :: Ord DistanceUnit

instance showDistanceUnit :: Show DistanceUnit where
  show Meters = "m"
  show Kilometers = "km"
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

-- | Create a constant `BigNumber` from a `String`
bigNum :: String -> BigNumber
bigNum = Generic.to <<< Constructor <<< Argument

-- | Given a number with the first unit, return what you need to multiply that number by to
-- | produce a quantity in the second unit.
convertBaseUnit :: BaseUnit -> BaseUnit -> Maybe BigNumber
convertBaseUnit a b
  | a == b = Just (bigNum "1.0")

convertBaseUnit a b =
  let
    ratios :: Array { from :: BaseUnit, to :: BaseUnit, ratio :: BigNumber }
    ratios =
      [ { from: Distance Meters, to: Distance Feet, ratio: bigNum "3.28084" }
      , { from: Distance Kilometers, to: Distance Meters, ratio: bigNum "1000" }
      , { from: Time Hours, to: Time Seconds, ratio: bigNum "3600" }
      ]

    first :: forall a. Array (Lazy (Maybe a)) -> Maybe a
    first a' = do
      { head, tail } <- Array.uncons a'
      Lazy.force head <|> first tail

    bfs :: BaseUnit -> BigNumber -> Set BaseUnit -> Maybe BigNumber
    bfs cur acc visited
      | cur == b = Just acc

    bfs cur acc visited =
      let
        neighbors :: Array (Tuple BaseUnit BigNumber)
        neighbors =
          []
            <> ((\i -> Tuple i.to i.ratio) <$> Array.filter (\{ from, to, ratio } -> from == cur) ratios)
            <> ((\i -> Tuple i.from (DivisionRing.recip i.ratio)) <$> Array.filter (\{ from, to, ratio } -> to == cur) ratios)

        unvisitedNeighbors = Array.filter (\(Tuple neighbor ratio) -> not (Set.member neighbor visited)) neighbors

        visited' = Set.insert cur visited
      in
        first $ Lazy.defer <$> (\(Tuple to ratio) _ -> bfs to (acc * ratio) visited') <$> unvisitedNeighbors
  in
    bfs a (bigNum "1.0") (Set.empty)

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

-- | Multiplies the given `CompUnit`s together.
times :: CompUnit -> CompUnit -> CompUnit
times (CompUnit { num: n1, den: d1 }) (CompUnit { num: n2, den: d2 }) = CompUnit { num: n1 <> n2, den: d1 <> d2 }

-- | Divides the first `CompUnit` by the second.
div :: CompUnit -> CompUnit -> CompUnit
div c1 c2 = c1 `times` (inverse c2)

-- | Returns the `CompUnit` to the nth power.
pow :: CompUnit -> Int -> CompUnit
pow c1 n =
  let
    repeat :: forall f a. Foldable f => Monoid (f a) => Int -> f a -> f a
    repeat n' item = rec n' mempty
      where
      rec 0 acc = acc

      rec n'' acc = rec (n'' - 1) (acc <> item)

    repeatCompUnit (CompUnit { num, den }) = CompUnit { num: repeat n num, den: repeat n den }
  in
    if n >= 0 then
      repeatCompUnit c1
    else
      repeatCompUnit (inverse c1)

-- | Returns the multiplicative inverse of the given unit (num and den flipped).
inverse :: CompUnit -> CompUnit
inverse (CompUnit { num, den }) = CompUnit { num: den, den: num }

-- | Given a number with the first unit, return what you need to multiply that number by to
-- | produce a quantity in the second unit.
convertCompUnit :: CompUnit -> CompUnit -> Maybe BigNumber
convertCompUnit (CompUnit { num: n1, den: d1 }) (CompUnit { num: n2, den: d2 }) =
  let
    convertList :: List BaseUnit -> List BaseUnit -> Maybe BigNumber
    convertList Nil Nil = Just (bigNum "1.0")

    convertList (Cons from tail1) (Cons to tail2) = (*) <$> (convertBaseUnit from to) <*> (convertList tail1 tail2)

    convertList _ _ = Nothing
  in
    do
      numeratorRatio <- convertList (List.fromFoldable n1) (List.fromFoldable n2)
      denominatorRatio <- convertList (List.fromFoldable d1) (List.fromFoldable d2)
      pure $ numeratorRatio / denominatorRatio

instance showCompUnit :: Show CompUnit where
  show (CompUnit { num, den }) = show num <> "/" <> show den

-- | Represents a Dimenseioned value - a number and a corresponding unit.
newtype DValue
  = DValue { scalar :: Number, unit :: CompUnit }
