{-# LANGUAGE DeriveAnyClass #-}
module ActorPi.Syntax where

import ActorPi.Context


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
  | Cse n [(n, Process b n)]
  | Bhv (Behavior b n)
  deriving (Show)
