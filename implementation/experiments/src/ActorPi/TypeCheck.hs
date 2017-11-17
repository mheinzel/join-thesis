module ActorPi.TypeCheck where

import           Data.List          (delete, nub)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as S
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Control.Monad      (when, unless, foldM)
import           Control.Error.Util (note)

import ActorPi.Syntax
import ActorPi.Context


data TypeError n =
    NotLocal n
  | CtxShape (Context n)
  | ChTooLong [n]
  | IncompatCtx (Context n) (Context n)
  | NotUnique (Set n)
  | InvInst
  deriving (Show)

data Error b n =
    Assertion String
  | Err (Process b n) (TypeError n)
  deriving (Show)


dropHeadCh :: Ord n => n -> [n] -> Maybe [n]
dropHeadCh x ns | x `notElem` drop 1 ns = Just (delete x ns)
dropHeadCh _ _ = Nothing


typeInfer :: Ord n => Process b n -> Either (Error b n) (Context n)
typeInfer Null    = return emptyContext
typeInfer (Snd _) = return emptyContext

typeInfer proc@(Pre (Recv x y) p) = do
  f <- typeInfer p
  let rho = domain f

  when (y `S.member` rho) $ Left (Err proc (NotLocal y))
  z <- note (Err proc (CtxShape f)) $ dropHeadCh x =<< asCh f
  note (Err proc (ChTooLong (x : z))) $ ch (x : z)

typeInfer proc@(Cse x cases) = do
  fs <- traverse (typeInfer . snd) cases
  let checkCompatibility f1 f2 =
        unless (isCompatible f1 f2) $ Left (Err proc (IncompatCtx f1 f2))
  sequence_ $ checkCompatibility <$> fs <*> fs
  note (Assertion "compat") $ foldM combine emptyContext fs

typeInfer proc@(Par p1 p2) = do
  f1 <- typeInfer p1
  f2 <- typeInfer p2
  let intersection = domain f1 `S.intersection` domain f2
  unless (S.null intersection) $ Left (Err proc (NotUnique intersection))
  note (Err proc (IncompatCtx f1 f2)) $ combine f1 f2

typeInfer proc@(New x p) = do
  f <- typeInfer p
  return $ restrictTo (domain f \\ S.singleton x) f

typeInfer proc@(Bhv (Behavior b x y)) = do
  unless (x == nub x) $ Left (Err proc InvInst)
  note (Err proc InvInst) $ ch x
