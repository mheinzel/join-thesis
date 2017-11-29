{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module ActorPi.Syntax where

import           Data.Monoid           ((<>))
import           Data.Maybe            (maybeToList)
import           Data.Set              (Set, (\\))
import qualified Data.Set              as S
import           Data.Functor.Foldable (Fix(..), cata)
import           Control.Monad         (when)
import           Control.Error.Util    (note)

import           Text.Show.Deriving    (deriveShow1)


data Recv n = Recv n [n]
  deriving (Show)
data Send n = Send n [n]
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
  deriving (Show)

recipients :: Definition b n -> [n]
recipients (Def _ (Recv x _) mX _ _) = x : maybeToList mX

definedProcess :: Definition b n -> Process b n
definedProcess d = firstRecv d .- continuation d


freeNames :: Ord n => Process b n -> Set n
freeNames = cata namesAlg
  where
    set = S.fromList
    namesAlg proc = case proc of
      Null -> set []
      Pre (Recv x ys) p -> set [x] <> (p \\ set ys)
      Snd (Send x ys) -> set (x : ys)
      New x p -> p \\ set [x]
      Par p1 p2 -> p1 <> p2
      Cse x cases -> set (x : fmap fst cases) <> foldMap snd cases
      Bhv (Behavior _ xs ys) -> set (xs ++ ys)

oneOrEmpty :: [a] -> Maybe (Maybe a)
oneOrEmpty [] = Just Nothing
oneOrEmpty [x] = Just (Just x)
oneOrEmpty _ = Nothing

data DefErrorReason b n
  = NoReceive n
  | NumberOfXs [n]
  | FreeNames [n] [n]
  deriving (Show)

-- error message?
define
  :: Ord n
  => b
  -> [n]
  -> [n]
  -> Process b n
  -> Either (DefErrorReason b n) (Definition b n)
define b (x:xs) ys p@(Fix (Pre (Recv x1 z) cont)) = do
  when (x /= x1) $ Left (NoReceive x)
  mX2 <- note (NumberOfXs (x:xs)) $ oneOrEmpty xs
  let vars = x : xs ++ ys

  when (S.fromList vars /= freeNames p) $ Left $ FreeNames vars $ S.toList $ freeNames p
  return $ Def b (Recv x1 z) mX2 ys cont
define _ (x:_) _ _ = Left (NoReceive x)
define _ [] _ _ = Left (NumberOfXs [])



-- short-hand notations:

nullproc :: Process b n
nullproc = Fix Null

recv :: n -> [n] -> Recv n
recv = Recv

(.-) :: Recv n -> Process b n -> Process b n
s .- p = Fix (Pre s p)
infixr 5 .-

send :: n -> [n] -> Process b n
send x ys = Fix (Snd (Send x ys))

new :: n -> Process b n -> Process b n
new x p = Fix (New x p)

(.|) :: Process b n -> Process b n -> Process b n
p .| q = Fix (Par p q)
infixr 2 .|

(~:) :: n -> Process b n -> (n, Process b n)
(~:) = (,)
infix 1 ~:

caseof :: n -> [(n, Process b n)] -> Process b n
caseof x cs = Fix (Cse x cs)

become :: b -> [n] -> [n] -> Process b n
become b xs ys = Fix (Bhv (Behavior b xs ys))
