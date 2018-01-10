module ActorPi.Scripts where

import ActorPi.Syntax
import ActorPi.Context
import ActorPi.TypeCheck
import ActorPi.Show
import ActorPi.LaTeX


-- TODO: temporary, move to `Program`
checkDefinition :: Definition String String -> Either String ()
checkDefinition def = do
  is <- either (Left . showTypeError) Right $ typeInfer (definedProcess def)
  need <- maybe (Left "ch") Right $ ch (recipients def)
  if is == need
    then return ()
    else Left $ unwords
      ["invalid definition"
      , show (behaviorName def) ++ ":"
      , showF is
      ]

-- TODO: temporary, move checking to `Program`
checkAndDumpProg :: (FilePath, Process String String, [Definition String String]) -> IO ()
checkAndDumpProg (fp, proc, defs) = do
  putStrLn $ either showTypeError (("type: " ++) . showF) $ typeInfer proc
  either putStrLn return $ mapM_ checkDefinition defs
  dumpLatex fp $ mathLines $ processToLatex proc : map definitionToLatex defs


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
