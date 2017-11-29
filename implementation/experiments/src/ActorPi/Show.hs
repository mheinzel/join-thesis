module ActorPi.Show where

import           Data.List (intercalate)
import           Data.Foldable (toList)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Functor.Foldable

import ActorPi.Syntax
import ActorPi.Context
import ActorPi.TypeCheck


parens, indent :: String -> String
parens x = "(" <> x <> ")"
indent = ("  " <>)

showIn :: String -> String -> String -> [String] -> String
showIn left sep right xs = left <> intercalate sep xs <> right

showCh :: [String] -> String
showCh = showIn "ch(" ", " ")"

showStar :: (a -> String) -> Star a -> String
showStar f star = case star of
  Star -> "*"
  Bott -> "_|_"
  N x  -> f x


showProcess :: Process String String -> String
showProcess proc = cata alg proc (0 :: Int)
  where
    parensPrec outer inner = if outer > inner then parens else id

    alg Null _ =
      "0"
    alg (Snd (Send x ys)) _ =
      x <> "!" <> intercalate "," ys
    alg (Pre (Recv x ys) p) prec =
      parensPrec prec 5 (x <> "(" <> intercalate "," ys <> ") . " <> p 5)
    alg (New x p) _ =
      "(new " <> x <> ") " <> p 4
    alg (Par p1 p2) prec =
      parensPrec prec 3 (p1 3 <> " | " <> p2 3)
    alg (Cse x cases) _ =
      "case " <> x <> " of " <> showCases cases
        where
          showCases = showIn "(" ", " ")" . fmap showCase
          showCase (y, p) = y <> ": " <> p 0
    alg (Bhv (Behavior b us vs)) _ =
      b <> "<" <> intercalate "," us <> ";" <> intercalate "," vs <> ">"


showDefinition :: Definition String String -> String
showDefinition d =
  behaviorName d <> " = " <> args <> " " <> showProcess (definedProcess d)
  where
    args = parens $ intercalate "," (recipients d)
          <> ";" <> intercalate "," (paramsY d)


showF :: Context String -> String
showF f = case asCh f of
  Just ns -> showCh ns
  Nothing -> showIn "{" ", " "}" (showAssoc <$> assocs f)
    where
      showAssoc (x, fx) = showIn "(" ", " ")" [x, showStar id fx]


showJudgement :: Judgement String String -> String
showJudgement (Judgement f p) = showF f <> "  |--  " <> showProcess p


showErrorReason :: TypeErrorReason String -> String
showErrorReason e = case e of
  Assertion str ->
    "Internal assertion failed: " <> str
  NotLocal ns ->
    "ACT: The names " <> show ns <> " are not local"
  ChShape f ->
    "ACT: Renaming " <> showF f <> " is not ch()-shaped"
  ChShapeX n ns ->
    "ACT: Renaming " <> showCh ns <> " is invalid when receiving on " <> show n
  ChTooLong ns ->
    "ACT: The resulting renaming " <> showCh ns <> " would be too long"
  IncompatCtxCASE f1 f2 ->
    "CASE: Incompatible renamings: " <> showF f1 <> " and " <> showF f2
  IncompatCtxCOMP f1 f2 ->
    "COMP: Incompatible renamings: " <> showF f1 <> " and " <> showF f2
  NotUnique ns ->
    "COMP: Some recipients are not unique: " <> intercalate ", " (S.toList ns)
  InvInst ->
    "INST: Invalid behavior instantiation"


showJudgementCtx :: JudgementCtx String String -> String
showJudgementCtx ctx =
  unlines (indent (showProcess p) : "in context:" : ctxDescription)
  where
    p = Fix (judgementProc <$> ctx)
    ctxDescription = indent . showJudgement <$> toList ctx


showTypeError :: TypeError String String -> String
showTypeError (TypeError ctx reason) =
  unlines [showErrorReason reason, "in process:", showJudgementCtx ctx]
