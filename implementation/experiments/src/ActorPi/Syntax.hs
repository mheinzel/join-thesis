{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module ActorPi.Syntax where

import           Data.Monoid           ((<>))
import           Data.Set              (Set, (\\))
import qualified Data.Set              as S
import           Data.Functor.Foldable (Fix(..), cata)
import           Text.Show.Deriving    (deriveShow1)

import ActorPi.Context


data Recv n = Recv n n
  deriving (Show)
data Send n = Send n n
  deriving (Show)
data Behavior b n = Behavior b [n] [n]
  deriving (Show)

data ProcessF b n p =
    Null
  | Pre (Recv n) p
  | Snd (Send n)
  | New n p
  | Par p p
  | Cse n [(n, p)]
  | Bhv (Behavior b n)
  deriving (Show, Functor, Foldable, Traversable)

deriveShow1 ''ProcessF  -- TODO

type Process b n = Fix (ProcessF b n)

-- always valid structure, but not necessarily type-correct
data Definition b n = Def
  { behaviorName :: b
  , firstRecv :: Recv n
  , secondX :: Maybe n
  , paramsY :: [n]
  , continuation :: Process b n
  }


freeNames :: Ord n => Process b n -> Set n
freeNames = cata namesAlg
  where
    set = S.fromList
    namesAlg proc = case proc of
      Null -> set []
      Pre (Recv x y) p -> set [x] <> (p \\ set [y])
      Snd (Send x y) -> set [x, y]
      New x p -> p \\ set [x]
      Par p1 p2 -> p1 <> p2
      Cse x cases -> set (x : fmap fst cases) <> foldMap snd cases
      Bhv (Behavior _ xs ys) -> set (xs ++ ys)

oneOrEmpty :: [a] -> Maybe (Maybe a)
oneOrEmpty [] = Just Nothing
oneOrEmpty [x] = Just (Just x)
oneOrEmpty _ = Nothing

-- error message?
define :: Ord n => b -> ([n], [n]) -> Process b n -> Maybe (Definition b n)
define b params@(x:xs, ys) (Fix (Pre (Recv x1 z) p))
  | x == x1
  , Just x2 <- oneOrEmpty xs
  , S.fromList (uncurry (++) params) == freeNames p
  = Just $ Def b (Recv x1 z) x2 ys p
define _ _ _ = Nothing


nullproc = Fix Null
recv = Recv
s .- p = Fix (Pre s p)
infixr 2 .-
send x y = Fix (Snd (Send x y))
new x p = Fix (New x p)
(~:) = (,)
infix 1 ~:
caseof x cs = Fix (Cse x cs)
become b xs ys = Fix (Bhv (Behavior b xs ys))
