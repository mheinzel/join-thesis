module ActorPi.TypeCheck where

import           Data.List          (delete, nub)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as S
import           Control.Monad      (when, unless, foldM)
import           Control.Error.Util (note)

import           Control.Comonad.Cofree (Cofree)

import Util.Recursion
import ActorPi.Syntax
import ActorPi.Context


data TypeErrorReason n =
    Assertion String
  | NotLocal n
  | ChShape (Context n)
  | ChShapeX n [n]
  | ChTooLong [n]
  | IncompatCtx (Context n) (Context n)
  | NotUnique (Set n)
  | InvInst
  deriving (Show)

data TypeError b n = TypeError
  { errorContext :: JudgementCtx b n
  , errorReason :: TypeErrorReason n
  }
  deriving (Show)

type JudgementCtx b n = ProcessF b n (Process b n, Context n)
type Prooftree b n = Cofree (ProcessF b n) (Context n)


infer
  :: Ord n
  => Process b n
  -> Either (TypeErrorReason n) (Context n)
infer = cataM typeInferAlg

inferTree
  :: Ord n
  => Process b n
  -> Either (TypeErrorReason n) (Prooftree b n)
inferTree = annotateCataM typeInferAlg

inferErrCtx
  :: Ord n
  => Process b n
  -> Either (TypeError b n) (Context n)
inferErrCtx = paraM (withErrCtx TypeError typeInferAlg)

inferTreeErrCtx
  :: Ord n
  => Process b n
  -> Either (TypeError b n) (Prooftree b n)
inferTreeErrCtx = annotateParaM (withErrCtx TypeError typeInferAlg)


dropHeadCh :: Eq n => n -> [n] -> Maybe [n]
dropHeadCh x ns = if x `elem` drop 1 ns
                     then Nothing
                     else Just (delete x ns)

typeInferAlg
  :: Ord n
  => ProcessF b n (Context n)
  -> Either (TypeErrorReason n) (Context n)

typeInferAlg Null = return emptyContext

typeInferAlg (Snd _) = return emptyContext

typeInferAlg (Pre (Recv x y) f) = do
  let rho = domain f

  when (y `S.member` rho) $ Left (NotLocal y)
  xz <- note (ChShape f) $ asCh f
  z <- note (ChShapeX x xz) $ dropHeadCh x xz
  note (ChTooLong (x : z)) $ ch (x : z)

typeInferAlg (Cse _ cases) = do
  let fs = fmap snd cases
  let checkCompatibility f1 f2 =
        unless (isCompatible f1 f2) $ Left (IncompatCtx f1 f2)
  sequence_ $ checkCompatibility <$> fs <*> fs
  note (Assertion "compat") $ foldM combine emptyContext fs

typeInferAlg (Par f1 f2) = do
  let intersection = domain f1 `S.intersection` domain f2
  unless (S.null intersection) $ Left (NotUnique intersection)
  note (IncompatCtx f1 f2) $ combine f1 f2

typeInferAlg (New x f) =
  return $ restrictTo (domain f \\ S.singleton x) f

typeInferAlg (Bhv (Behavior _ x _)) = do
  unless (x == nub x) $ Left InvInst
  note InvInst $ ch x
