module Exponentials where

import Prelude
import Data.Foldable as Foldable
import Data.Group (class Group, ginverse)
import Data.Group as Group
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.String as String
import Data.Tuple (Tuple(..))

-- | Represents a collection of values that are raised to positive or negative integer exponents.
-- | Transparently handles eliminating zero-power values.
newtype Exponentials a
  = Exponentials (Map a Int)

derive instance newtypeExponentials :: Newtype (Exponentials a) _

instance showExponentialsString :: Show (Exponentials String) where
  show e@(Exponentials m) =
    let
      showPower :: Tuple String Int -> String
      showPower (Tuple a 1) = a

      showPower (Tuple a n) = a <> "^" <> show n
    in
      String.joinWith "*" (showPower <$> toArray e)
else instance showExponentials :: (Ord a, Show a) => Show (Exponentials a) where
  show e@(Exponentials m) =
    let
      showPower :: Tuple a Int -> String
      showPower (Tuple a 1) = show a

      showPower (Tuple a n) = show a <> "^" <> show n
    in
      String.joinWith "*" (showPower <$> toArray e)

cancel :: forall a. Ord a => Exponentials a -> Exponentials a
cancel = Newtype.over Exponentials $ Map.filter ((/=) 0)

instance eqExponentials :: Ord a => Eq (Exponentials a) where
  eq :: Exponentials a -> Exponentials a -> Boolean
  eq e1 e2 = (Newtype.unwrap e1) == (Newtype.unwrap e2)

instance semigroupExponentials :: Ord a => Semigroup (Exponentials a) where
  append (Exponentials e1) (Exponentials e2) = cancel $ Exponentials $ Map.unionWith (+) e1 e2

instance monoidExponentials :: Ord a => Monoid (Exponentials a) where
  mempty = Exponentials mempty

instance groupExponentials :: Ord a => Group (Exponentials a) where
  ginverse :: forall a. Exponentials a -> Exponentials a
  ginverse = Newtype.over Exponentials (\m -> negate <$> m)

-- | Array representation of all of the exponential elements.
toArray :: forall a. Exponentials a -> Array (Tuple a Int)
toArray = Map.toUnfoldableUnordered <<< Newtype.unwrap

-- | Standard left fold.
fold :: forall a b. (b -> Tuple a Int -> b) -> b -> Exponentials a -> b
fold f init = Foldable.foldl f init <<< toArray

-- | Similar to 'foldl', but the result is encapsulated in a monad.
foldM :: forall a b m. Monad m => (b -> Tuple a Int -> m b) -> b -> Exponentials a -> m b
foldM f init = Foldable.foldM f init <<< toArray

-- | Transform values. Note that we cannot derive Functor because b must be Ord.
map :: forall a b. Ord b => (a -> b) -> Exponentials a -> Exponentials b
map f e =
  Exponentials
    $ Map.fromFoldable
    $ (\(Tuple k v) -> Tuple (f k) v)
    <$> (toArray e)

-- | Wraps a single value a, denoting a^1.
singleton :: forall a. a -> Exponentials a
singleton a = Exponentials $ Map.singleton a 1

-- | Given an array of numerator and denominator elements, return the corresponding Exponential.
quotient :: forall a. Ord a => Array a -> Array a -> Exponentials a
quotient num den =
  let
    foldProduct p = Foldable.foldl (<>) mempty $ singleton <$> p
  in
    (foldProduct num) <> (ginverse $ foldProduct den)

-- | Raise a to the given exponent.
power :: forall a. Ord a => a -> Int -> Exponentials a
power a n = Group.power (singleton a) n
