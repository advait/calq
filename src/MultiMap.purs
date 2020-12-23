module MultiMap where

import Prelude
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

-- | Map with `Monoid` values.
newtype MultiMap k m
  = MultiMap (Map k m)

-- | A `MultiMap` is a `Semigroup` where we simply combine the monoids.
instance semigroupMultiMap :: (Ord k, Monoid m) => Semigroup (MultiMap k m) where
  append :: MultiMap k m -> MultiMap k m -> MultiMap k m
  append m1 (MultiMap m2) =
    let
      items = (Map.toUnfoldable m2 :: Array (Tuple k m))
    in
      foldl (\m' (Tuple k v) -> insert k v m') m1 items

instance monoidMultiMap :: (Ord k, Monoid m) => Monoid (MultiMap k m) where
  mempty = MultiMap mempty

-- | Looks up the given item in the map, returning `mempty` if there were no prior inserts.
lookup :: forall k m. Ord k => Monoid m => k -> MultiMap k m -> m
lookup key (MultiMap map) = mempty `fromMaybe` Map.lookup key map

-- | Inserts the given item in the map, combining `Monoid` values.
insert :: forall k m. Ord k => Monoid m => k -> m -> MultiMap k m -> MultiMap k m
insert key value map'@(MultiMap map) = MultiMap $ Map.insert key ((lookup key map') <> value) map

-- | Groups items with the MultiMap.
groupBy :: forall a t k v. Ord k => Traversable t => Monoid v => (a -> Tuple k v) -> t a -> MultiMap k v
groupBy f items =
  let
    kvPairs = f <$> items
  in
    foldl (\m (Tuple k v) -> insert k v m) mempty kvPairs
