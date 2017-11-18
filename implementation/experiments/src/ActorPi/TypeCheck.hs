{-# LANGUAGE TupleSections #-}
module ActorPi.TypeCheck where

import           Data.List          (delete, nub)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as S
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Bifunctor     (first)
import           Control.Monad      (when, unless, foldM, guard)
import           Control.Error.Util (note)

import           Control.Comonad.Cofree
import           Data.Functor.Foldable

import Util.Recursion
import ActorPi.Syntax
import ActorPi.Context


data TypeError n =
    Assertion String
  | NotLocal n
  | CtxShape (Context n)
  | ChTooLong [n]
  | IncompatCtx (Context n) (Context n)
  | NotUnique (Set n)
  | InvInst
  deriving (Show)

type Prooftree b n = Cofree (ProcessF b n) (Context n)


infer
  :: Ord n
  => Process b n
  -> Either (TypeError n) (Context n)
infer = cataM typeInferAlg

inferTree
  :: Ord n
  => Process b n
  -> Either (TypeError n) (Prooftree b n)
inferTree = annotateM typeInferAlg

inferErrCtx
  :: Ord n
  => Process b n
  -> Either (Process b n, TypeError n) (Context n)
inferErrCtx = paraM $ (\(btt,p) -> first (Fix btt, ) (typeInferAlg p)) . split
  where
    split x = (fst <$> x, snd <$> x)


typeInferAlg
  :: Ord n
  => ProcessF b n (Context n)
  -> Either (TypeError n) (Context n)

typeInferAlg Null    = return emptyContext

typeInferAlg (Snd _) = return emptyContext

typeInferAlg (Pre (Recv x y) f) = do
  let rho = domain f
      dropHeadCh x ns = guard (x `notElem` drop 1 ns) *> Just (delete x ns)

  when (y `S.member` rho) $ Left (NotLocal y)
  -- TODO: separate error types
  z <- note (CtxShape f) $ dropHeadCh x =<< asCh f
  note (ChTooLong (x : z)) $ ch (x : z)

typeInferAlg (Cse x cases) = do
  let fs = fmap snd cases
  let checkCompatibility f1 f2 =
        unless (isCompatible f1 f2) $ Left (IncompatCtx f1 f2)
  sequence_ $ checkCompatibility <$> fs <*> fs
  note (Assertion "compat") $ foldM combine emptyContext fs

typeInferAlg proc@(Par f1 f2) = do
  let intersection = domain f1 `S.intersection` domain f2
  unless (S.null intersection) $ Left (NotUnique intersection)
  note (IncompatCtx f1 f2) $ combine f1 f2

typeInferAlg proc@(New x f) = do
  return $ restrictTo (domain f \\ S.singleton x) f

typeInferAlg proc@(Bhv (Behavior b x y)) = do
  unless (x == nub x) $ Left InvInst
  note InvInst $ ch x
