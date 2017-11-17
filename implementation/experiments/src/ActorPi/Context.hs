module ActorPi.Context
  ( Star (..)
  , Context
  , emptyContext
  , domain
  , restrictTo
  , isCompatible
  , combine
  , ch
  , asCh
  ) where

import           Data.Maybe         (isJust, fromMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Map           (Map)
import qualified Data.Map           as M


data Star n = N n | Star | Bott
  deriving (Show, Eq)

newtype Context n = Ctx (Map n (Star n))
  deriving (Show, Eq)


emptyContext :: Context n
emptyContext = Ctx M.empty

domain :: Ord n => Context n -> Set n
domain (Ctx m) = S.fromList (M.keys m)

restrictTo :: Ord n => Set n -> Context n -> Context n
restrictTo dom (Ctx m) = Ctx $ M.filterWithKey (\k _ -> k `S.member` dom) m

assocs :: Context n -> [(n, Star n)]
assocs (Ctx m) = M.assocs m

lookupContext :: Ord n => n -> Context n -> Maybe (Star n)
lookupContext n (Ctx m) = M.lookup n m


isValid :: Ord n => Context n -> Bool
isValid ctx =
  and [f x /= N x | x <- rho]
  && and [fstar (f x) == Bott | x <- rho]
  && and [f x /= f y || f x `elem` [Bott, Star] || x == y | x <- rho, y <- rho]
  where
    rho = S.toList $ domain ctx
    f n = fromMaybe (error "isValid") $ lookupContext n ctx
    fstar (N n) = f n
    fstar _ = Bott

isCompatible :: Ord n => Context n -> Context n -> Bool
isCompatible f1 f2 =
  let f1f2 = combine f1 f2
      f2f1 = combine f2 f1
   in isJust f1f2 && f1f2 == f2f1

combine :: Ord n => Context n -> Context n -> Maybe (Context n)
combine (Ctx m) (Ctx n) =
  let f = Ctx $ M.unionWith (\y1 y2 -> if y1 /= Bott then y1 else y2) m n
   in if isValid f then Just f else Nothing


ch :: Ord n => [n] -> Maybe (Context n)
ch []    = Just emptyContext
ch [x]   = Just $ Ctx $ M.fromList [(x, Bott)]
ch [x,y] = Just $ Ctx $ M.fromList [(x, N y), (y, Bott)]
ch _     = Nothing

asCh :: Ord n => Context n -> Maybe [n]
asCh f = case assocs f of
  [] -> Just []
  [(x, Bott)] -> Just [x]
  [(x, fx), (y, Bott)] | fx == N y -> Just [x, y]
  [(x, Bott), (y, fy)] | fy == N x -> Just [y, x]
  _ -> Nothing
