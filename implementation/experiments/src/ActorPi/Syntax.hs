{-# LANGUAGE DeriveAnyClass #-}
module ActorPi.Syntax where

import           Data.Bool          (bool)
import           Data.Maybe         (isJust, fromMaybe)
import           Data.Monoid        ((<>))
import           Data.List          (delete, nub)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as S
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Control.Monad      (when, unless, foldM)
import           Control.Error.Util (note)

data Recv n = Recv n n
  deriving (Show)
data Send n = Send n n
  deriving (Show)
data Behavior b n = Behavior b [n] [n]
  deriving (Show)

data Process b n =
    Null
  | Pre (Recv n) (Process b n)
  | Snd (Send n)
  | New n (Process b n)
  | Par (Process b n) (Process b n)
  | Cse n [(n, (Process b n))]
  | Bhv (Behavior b n)
  deriving (Show)

data Star n = N n | Star | Bott
  deriving (Show, Eq)

data Context n = Ctx (Map n (Star n))
  deriving (Show, Eq)

emptyContext :: Context n
emptyContext = Ctx M.empty

ch :: Ord n => [n] -> Maybe (Context n)
ch []    = Just emptyContext
ch [x]   = Just $ Ctx $ M.fromList [(x, Bott)]
ch [x,y] = Just $ Ctx $ M.fromList [(x, N y), (y, Bott)]
ch _     = Nothing

lookupCtx :: Ord n => n -> Context n -> Maybe (Star n)
lookupCtx n (Ctx m) = M.lookup n m

domain :: Ord n => Context n -> Set n
domain (Ctx m) = S.fromList (M.keys m)

assocs :: Context n -> [(n, Star n)]
assocs (Ctx m) = M.assocs m

isValid :: Ord n => Context n -> Bool
isValid ctx =
  and [f x /= N x | x <- rho]
  && and [fstar (f x) == Bott | x <- rho]
  && and [f x /= f y || f x `elem` [Bott, Star] || x == y | x <- rho, y <- rho]
  where
    rho = S.toList $ domain ctx
    f n = fromMaybe (error "isValid") $ lookupCtx n ctx
    fstar (N n) = f n
    fstar _ = Bott

combine :: Ord n => Context n -> Context n -> Maybe (Context n)
combine (Ctx m) (Ctx n) =
  let f = Ctx $ M.unionWith (\y1 y2 -> if y1 /= Bott then y1 else y2) m n
   in if isValid f then Just f else Nothing

compatible :: Ord n => Context n -> Context n -> Bool
compatible f1 f2 =
  let f1f2 = combine f1 f2
      f2f1 = combine f2 f1
   in isJust f1f2 && f1f2 == f2f1

restrictTo :: Ord n => Set n -> Context n -> Context n
restrictTo dom (Ctx m) = Ctx $ M.filterWithKey (\k _ -> k `S.member` dom) m


data Judgement b n = Context n :|- Process b n

asCh :: Ord n => Context n -> Maybe [n]
asCh f = case assocs f of
  [] -> Just []
  [(x, Bott)] -> Just [x]
  [(x, fx), (y, Bott)] | fx == N y -> Just [x, y]
  [(x, Bott), (y, fy)] | fy == N x -> Just [y, x]
  _ -> Nothing


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
dropHeadCh x ns | not (x `elem` drop 1 ns) = Just (delete x ns)
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
        unless (compatible f1 f2) $ Left (Err proc (IncompatCtx f1 f2))
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

