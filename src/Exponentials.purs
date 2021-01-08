module Exponentials where

import Prelude
import Data.Foldable as Foldable
import Data.Group (class Group, ginverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))

newtype Exponentials a
  = Exponentials (Map a Int)

derive instance newtypeExponentials :: Newtype (Exponentials a) _

instance showExponentials :: (Ord a, Show a) => Show (Exponentials a) where
  show (Exponentials m) =
    let
      num = Map.toUnfoldableUnordered $ Map.filter (\n -> n > 0) m

      den = Map.toUnfoldableUnordered $ negate <$> Map.filter (\n -> n < 0) m
    in
      case Tuple num den of
        Tuple [] [] -> ""
        Tuple [] den' -> "1/" <> show den'
        Tuple num' [] -> show num'
        Tuple num' den' -> show num' <> "/" <> show den'

-- | "Cancel out" values with exponent of zero.
cancel :: forall a. Ord a => Exponentials a -> Exponentials a
cancel = Newtype.over Exponentials $ Map.filter ((/=) 0)

instance eqExponentials :: Ord a => Eq (Exponentials a) where
  eq :: Exponentials a -> Exponentials a -> Boolean
  eq e1 e2 = (Newtype.unwrap $ cancel e1) == (Newtype.unwrap $ cancel e2)

instance semigroupExponentials :: Ord a => Semigroup (Exponentials a) where
  append (Exponentials e1) (Exponentials e2) = cancel $ Exponentials $ Map.unionWith (+) e1 e2

instance monoidExponentials :: Ord a => Monoid (Exponentials a) where
  mempty = Exponentials mempty

instance groupExponentials :: Ord a => Group (Exponentials a) where
  ginverse :: forall a. Exponentials a -> Exponentials a
  ginverse = Newtype.over Exponentials (\m -> negate <$> m)

-- | Standard left fold.
fold :: forall a b. (b -> Tuple a Int -> b) -> b -> Exponentials a -> b
fold f init e =
  let
    items = Map.toUnfoldableUnordered $ Newtype.unwrap e :: Array (Tuple a Int)
  in
    Foldable.foldl f init items

-- | Similar to 'foldl', but the result is encapsulated in a monad.
foldM :: forall a b m. Monad m => (b -> Tuple a Int -> m b) -> b -> Exponentials a -> m b
foldM f init e =
  let
    items = Map.toUnfoldableUnordered $ Newtype.unwrap e :: Array (Tuple a Int)
  in
    Foldable.foldM f init items

-- | Transform values. Note that we cannot derive Functor because b must be Ord.
map :: forall a b. Ord b => (a -> b) -> Exponentials a -> Exponentials b
map f (Exponentials m) =
  let
    items = Map.toUnfoldableUnordered m :: Array (Tuple a Int)
  in
    Exponentials
      $ Map.fromFoldable
      $ (\(Tuple k v) -> Tuple (f k) v)
      <$> items

singleton :: forall a. a -> Exponentials a
singleton a = Exponentials $ Map.singleton a 1

quotient :: forall a. Ord a => Array a -> Array a -> Exponentials a
quotient num den =
  let
    foldProduct p = Foldable.foldl (<>) mempty $ singleton <$> p
  in
    (foldProduct num) <> (ginverse $ foldProduct den)
