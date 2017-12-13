{-# OPTIONS_GHC -w #-}
module ActorPi.Examples where

import ActorPi.Syntax

named p = become p [] []

paperP9 =
  paperP9left .| paperP9right
paperP9left =
  new ["x"]
    ( recv "x" ["u"] .- named "P_1"
   .| recv "y" ["v"] .- named "Q_1"
   .| send "x" ["y"]
   .| send "z" ["y"]
    )
paperP9right =
  new ["x"]
    ( recv "x" ["u"] .- named "P_2"
   .| recv "z" ["v"] .- named "Q_2"
   .| send "w" ["z"]
    )

caseSameActorDifferentTemp =
  caseof "u"
    [ "v" ~: recv "x" ["a"] .- become "B" ["z"] ["a"]
    , "w" ~: recv "y" ["b"] .- become "B" ["z"] ["b"]
    ]

usesEverything =
  recv "z" ["k"] .- send "k" ["a"] .| caseof "u"
    [ "v" ~: new ["k"] (send "z" ["k"] .| recv "k" ["y"] .- nullproc)
    , "w" ~: recv "x" ["b"] .- become "B" ["y"] ["b"]
    ]


encDef =
  new ["a","x","y"] $ become "B" ["a"] ["x","y"]
                   .| become "B_c" ["x"] ["a"]
                   .| become "B_c" ["y"] ["a"]

encDefB = either (error . show) id $
  define "B" ["a"] ["x","y"] $
    recv "a" ["c","i"] .- caseof "c"
      [ "x" ~: become "B_x" ["a"] ["x","y","i"]
      , "y" ~: become "B" ["a"] ["x","y"] .| send "a" ["y","i"]
      ]

encDefBx = either (error . show) id $
  define "B_x" ["a"] ["x","y","u"] $
    recv "a" ["c","v"] .- caseof "c"
      [ "x" ~: become "B_x" ["a"] ["x","y","v"] .| send "a" ["x","u"]
      , "y" ~: become "B" ["a"] ["x","y"] .| become "P" [] ["x","y","u","v"]
      ]

encDefBc = either (error . show) id $
  define "B_c" ["x"] ["a"] $
    recv "x" ["i"] .- (send "a" ["x","i"] .| become "B_c" ["x"] ["a"])
