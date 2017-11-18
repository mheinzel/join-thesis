{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Recursion where

import Data.Functor.Foldable
import Control.Monad (join)
import Control.Comonad.Cofree (Cofree(..))


cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a)
  -> t
  -> m a
cataM alg = cata (join . fmap alg . sequenceA)

paraM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t (t, a) -> m a)
  -> t
  -> m a
paraM alg = para (join . fmap alg . traverse sequenceA)

annotateM
  :: (Traversable f, Monad m)
  => (f a -> m a)
  -> Fix f
  -> m (Cofree f a)
annotateM alg = cataM (\fa -> (:< fa) <$> alg (label <$> fa))
  where
    label (l :< _) = l
