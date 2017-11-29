module ActorPi.Scripts where

import ActorPi.Syntax
import ActorPi.TypeCheck
import ActorPi.Show
import ActorPi.LaTeX


dumpProofTree :: FilePath -> Process String String -> IO ()
dumpProofTree path proc =
  either
    (putStrLn . showTypeError)
    (dumpLatex path . judgementTreeToLatex)
    (typeInferTree proc)

mathLines :: [String] -> String
mathLines strs = unlines (["\\begin{align*}"] ++ map (\x -> "& " ++ x ++ " \\\\") strs ++ ["\\end{align*}"])

dumpLatex :: FilePath -> String -> IO ()
dumpLatex path = writeFile path . standalone
  where
    standalone latex = unlines
      [ "\\documentclass{article}"
      , "\\usepackage{../../thesis/style/math}"
      , "\\usepackage[active,tightpage,displaymath]{preview}"
      , "\\nofiles"
      , "\\begin{document}\n"
      , latex
      , "\\end{document}"
      ]
