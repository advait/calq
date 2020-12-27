module MaybeWriter where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Morph (class MFunctor)
import Control.Monad.Writer (class MonadTell)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype MaybeWriter w a
  = MaybeWriter (Tuple (Maybe w) a)

runMaybeWriter :: forall w a. MaybeWriter w a -> Tuple (Maybe w) a
runMaybeWriter (MaybeWriter t) = t

derive instance newtypeMaybeWriter :: Newtype (MaybeWriter w a) _

instance functorMaybeWriter :: Functor (MaybeWriter mw) where
  map f (MaybeWriter (Tuple mw a)) = (MaybeWriter (Tuple mw (f a)))

instance applyMaybeWriter :: Apply (MaybeWriter mw) where
  apply :: forall a b. MaybeWriter mw (a -> b) -> MaybeWriter mw a -> MaybeWriter mw b
  apply (MaybeWriter (Tuple w1 f)) (MaybeWriter (Tuple w2 a)) = MaybeWriter (Tuple (w1 <|> w2) (f a))

instance applicativeMaybeWriter :: Applicative (MaybeWriter mw) where
  pure :: forall a. a -> MaybeWriter mw a
  pure a = MaybeWriter (Tuple Nothing a)

instance bindMaybeWriter :: Bind (MaybeWriter mw) where
  bind :: forall a b. MaybeWriter mw a -> (a -> MaybeWriter mw b) -> MaybeWriter mw b
  bind (MaybeWriter (Tuple mw1 a)) f =
    let
      (MaybeWriter (Tuple mw2 b)) = f a
    in
      MaybeWriter (Tuple (mw1 <|> mw2) b)

instance monadMaybeWriter :: Monad (MaybeWriter mw)

instance monadTellWriterT :: MonadTell w (MaybeWriter w) where
  tell :: forall w. w -> MaybeWriter w Unit
  tell w = MaybeWriter (Tuple (Just w) unit)

-- instance mFunctorWriterT :: MFunctor (MaybeWriter 
