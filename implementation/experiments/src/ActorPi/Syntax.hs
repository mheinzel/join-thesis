{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module ActorPi.Syntax where

import Data.Functor.Foldable (Fix(..))
import Text.Show.Deriving (deriveShow1)

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
