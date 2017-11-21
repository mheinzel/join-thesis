{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Util.Recursion where

import Data.Functor.Foldable
import Data.Bifunctor (first)
import Control.Monad (join)
import Control.Comonad.Cofree (Cofree(..))


headCofree :: Cofree f a -> a
headCofree (h :< _) = h


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

annotateCataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a)
  -> t
  -> m (Cofree (Base t) a)
annotateCataM alg = annotateParaM (alg . fmap snd)

annotateParaM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t (t, a) -> m a)
  -> t
  -> m (Cofree (Base t) a)
annotateParaM alg = paraM (\ftca -> (:< fmap snd ftca) <$> alg (fmap headCofree <$> ftca))

withErrCtx
  :: (a -> e -> e')
  -> (a -> Either e b)
  -> a -> Either e' b
withErrCtx err alg ctx = first (err ctx) (alg ctx)
