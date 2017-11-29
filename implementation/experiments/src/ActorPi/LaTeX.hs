module ActorPi.LaTeX where

import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Control.Comonad.Trans.Cofree (CofreeF((:<)))

import ActorPi.Syntax
import ActorPi.Context
import ActorPi.TypeCheck


showIn :: String -> String -> String -> [String] -> String
showIn left sep right xs = left <> intercalate sep xs <> right

starToLatex :: (a -> String) -> Star a -> String
starToLatex f star = case star of
  Star -> "\\ast"
  Bott -> "\\bot"
  N x  -> f x

macro :: String -> [String] -> String
macro name = showIn ("\\" <> name <> "{") "}{" "}"


processToLatex :: Process String String -> String
processToLatex proc = cata alg proc (0 :: Int)
  where
    parensPrec outer inner x = if outer > inner
                                  then macro "parens" [x]
                                  else x
    alg Null _ =
      macro "anullproc" []
    alg (Snd (Send x ys)) _ =
      macro "asnd" [x, intercalate "," ys]
    alg (Pre (Recv x ys) p) prec =
      parensPrec prec 5 (macro "arcv" [x, intercalate "," ys] <> " . " <> p 5)
    alg (New x p) _ =
      macro "anew" [x, p 4]
    alg (Par p1 p2) prec =
      parensPrec prec 3 (p1 3 <> " \\apar " <> p2 3)
    alg (Cse x alts) _ =
      macro "acse" [x, texAlts alts]
        where
          texAlts = intercalate ", " . fmap texAlt
          texAlt (y, p) = macro "aalt" [y, p 0]
    alg (Bhv (Behavior b us vs)) _ =
      macro "ains" [b, intercalate ", " us, intercalate ", " vs]


fToLatex :: Context String -> String
fToLatex f = case asCh f of
  Just ns | ns /= [] -> macro "ch" [intercalate "," ns]
  _ -> showIn "\\{" ", " "\\}" (assocToLatex <$> assocs f)
    where
      assocToLatex (x, fx) = showIn "(" ", " ")" [x, starToLatex id fx]

recipientsToLatex :: Context String -> String
recipientsToLatex = intercalate ", " . toList . domain

judgementToLatex :: Judgement String String -> String
judgementToLatex (Judgement t p) =
  macro "judgement" [recipientsToLatex t, fToLatex t, processToLatex p]


data JudgementStep b n = Step
  { stepArity :: Int
  , stepJudgement :: Judgement b n
  }
  deriving (Show)

linearizeJudgementTree :: JudgementTree b n -> [JudgementStep b n]
linearizeJudgementTree = cata alg
  where
    alg (j :< sub) = let subProofs = take 5 $ toList sub  -- limit of bussproof
                         arity = length subProofs
                      in concat subProofs ++ [Step arity j]

judgementTreeToLatex :: JudgementTree String String -> String
judgementTreeToLatex = wrap . fmap stepToLatex . linearizeJudgementTree
  where
    wrap ls = unlines (["\\begin{prooftree}"] ++ ls ++ ["\\end{prooftree}"])

    stepToLatex (Step arity j) =
      macro (command arity) ["$" <> judgementToLatex j <> "$"]

    command a = case a of
      0 -> "AxiomC"
      1 -> "UnaryInfC"
      2 -> "BinaryInfC"
      3 -> "TrinaryInfC"
      4 -> "QuaternaryInfC"
      5 -> "QuinaryInfC"
      _ -> error $ "invalid arity of " <> show a

