{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Recursion where

import Data.Functor.Foldable
import Data.Bifunctor (first)
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
annotateParaM alg = paraM (\ftca -> (:< fmap snd ftca) <$> alg (fmap label <$> ftca))
  where
    label (l :< _) = l

withErrCtx
  :: Functor (Base t)
  => (Base t (t, a) -> e -> e')
  -> (Base t a -> Either e a)
  -> Base t (t, a)
  -> Either e' a
withErrCtx f alg ctx = first (f ctx) $ alg (snd <$> ctx)
