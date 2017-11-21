module ActorPi.Scripts where

import ActorPi.Syntax
import ActorPi.TypeCheck
import ActorPi.Show
import ActorPi.LaTeX


dumpProofTree :: FilePath -> Process String String -> IO ()
dumpProofTree path proc =
  case standalone . judgementTreeToLatex <$> typeInferTree proc of
    Left err -> putStrLn (showTypeError err)
    Right latex -> writeFile path latex

  where
    standalone latex = unlines
      [ "\\documentclass[varwidth=\\maxdimen]{standalone}"
      , "\\usepackage{../../thesis/style/math}"
      , "\\nofiles"
      , "\\begin{document}\n"
      , latex
      , "\\end{document}"
      ]
