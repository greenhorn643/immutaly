{-# LANGUAGE RankNTypes #-}
module Data.Immutaly.Internal.Util.Lens
  ( (<%%~)
  , (%%%=)
  , (<%%%=)
  , (%%>)
  , Lens2
  , nestLens
  ) where
import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Immutaly.Internal.Util

(%%%=) :: Monad m => Lens' s a -> (a -> m a) -> StateT s m ()
(%%%=) l f = modifyM $ l %%~ f
infixr 4 %%%=

(<%%~) ::
  forall f s t a b. Functor f
  => Lens s t a b -> (a -> f b) -> s -> f (b, t)
(<%%~) l f = l' f'
  where l' = nestLens l
        f' = fmap dup . f
        dup a = (a, a)
infixr 4 <%%~

newtype NestedFunctor f g a = NestedFunctor
  { unnestedFunctor :: g (f a)
  }

instance (Functor f, Functor g) => Functor (NestedFunctor f g) where
  fmap f = NestedFunctor . (fmap . fmap) f . unnestedFunctor

type Lens2 s t a b =
  forall f g. (Functor f, Functor g)
  => (a -> g (f b)) -> s -> g (f t)

nestLens :: Lens s t a b -> Lens2 s t a b
nestLens l f = unnestedFunctor . l (NestedFunctor . f)

(<%%%=) :: Monad m => Lens' s a -> (a -> m a) -> StateT s m a
(<%%%=) l f = StateT $ l <%%~ f
infixr 4 <%%%=

(%%>) :: Monad m => Lens' s a -> (a -> m b) -> StateT s m b
(%%>) l f = get >>= lift . f . (^. l)
infixr 4 %%>