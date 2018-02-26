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


naiveEncoding =
  new ["x","y"] $ become "B" ["x","y"] []
               .| become "Q" [] ["x","y"]

naiveDef = either (error . show) id $
  define "B" ["x","y"] [] $
    recv "x" ["u"] .- recv "y" ["v"] .-
      ( become "B" ["x","y"] []
     .| become "P" [] ["x","y","u","v"]
      )


encDef =
  new ["a","x","y"] $ become "B" ["a"] ["x","y"]
                   .| become "B_{fw}" ["x"] ["a"]
                   .| become "B_{fw}" ["y"] ["a"]

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
  define "B_{fw}" ["x"] ["a"] $
    recv "x" ["i"] .- (send "a" ["x","i"] .| become "B_{fw}" ["x"] ["a"])



encList =
  new ["a","x","y","l_u","l_v"] $ become "B_a" ["a"] ["x","y","l_u","l_v"]
                               .| become "B_{fw}" ["x"] ["a"]
                               .| become "B_{fw}" ["y"] ["a"]
                               .| become "B_{nil}" ["l_u"] []
                               .| become "B_{nil}" ["l_v"] []
                               .| become "Q" [] ["x","y"]

encDefBnil = either (error . show) id $
  define "B_{nil}" ["l"] [] $
    recv "l" ["k_n","k_c"] .- send "k_n" []

encDefBcons = either (error . show) id $
  define "B_{cons}" ["l"] ["h","t"] $
    recv "l" ["k_n","k_c"] .- send "k_c" ["h","t"]

encDefBjoin = either (error . show) id $
  define "B_{join}" ["l"] ["u","v","l_v"] $
    (recv "l" ["r_n","r_c"] .-) $ new ["k_n","k_c"]
      $ send "l_v" ["k_n","k_c"]
     .| recv "k_n" [] .- (become "P" [] ["u","v"] .| send "r_n" [])  -- must join here
     .| recv "k_c" ["h_v","t_v"] .- caseof "u"  -- non-deterministic choice
        [ "u" ~: become "P" [] ["u","v"] .| send "r_c" ["h_v", "t_v"]
        , "u" ~: new ["l_{joined}"]
          ( become "B_{join}" ["l_{joined}"] ["u","h_v","t_v"]
         .| send "r_c" ["v", "l_{joined}"]  -- join in tail
          )
        ]

encDefBinsert = either (error . show) id $
  define "B_{insert}" ["t"] ["u","l_u","l_v"] $
    recv "t" ["k"] .- new ["k_c","k_n"]
      ( send "l_v" ["k_n","k_c"]
     .| recv "k_n" [] .- new ["l_{u,new}"]
         ( become "B_{cons}" ["l_{u,new}"] ["u","l_u"]
        .| send "k" ["l_{u,new}","l_v"])
     .| recv "k_c" ["h_v","t_v"] .- new ["l_{v,new}"]
         ( become "B_{join}" ["l_{v,new}"] ["u","h_v","t_v"]
        .| send "k" ["l_u","l_{v,new}"])
      )

encDefBa = either (error . show) id $
  define "B_a" ["a"] ["x","y","l_u","l_v"] $
    recv "a" ["c","i"] .- new ["k","t"]
      ( caseof "c"
        [ "x" ~: become "B_{insert}" ["t"] ["i","l_u","l_v"]
              .| send "t" ["k"]
              .| recv "k" ["l_{u,new}", "l_{v,new}"]
                   .- become "B_a" ["a"] ["x","y","l_{u,new}","l_{v,new}"]
        , "y" ~: become "B_{insert}" ["t"] ["i","l_v","l_u"]
              .| send "t" ["k"]
              .| recv "k" ["l_{v,new}", "l_{u,new}"]
                   .- become "B_a" ["a"] ["x","y","l_{u,new}","l_{v,new}"]
        ]
      )

listenc =
  ( "listenc.tex"
  , encList
  , [ encDefBc
    , encDefBnil
    , encDefBcons
    , encDefBa
    , encDefBinsert
    , encDefBjoin
    ]
  )

encNew =
  new ["a","x","y","l"] $ become "B_a" ["a"] ["x","y","x","l"]
                       .| become "B_{fw}" ["x"] ["a"]
                       .| become "B_{fw}" ["y"] ["a"]
                       .| become "B_{nil}" ["l"] []
                       .| become "Q" [] ["x","y"]

encNewDefBa = either (error . show) id $
  define "B_a^P" ["a"] ["x","y","f","l"] $
    recv "a" ["c","p"] .- new ["k_n","k_c","k_P","k_a"]
      ( send "l" ["k_n","k_c"]
     .| recv "k_n" [] .- new ["l'"]
          ( become "B_{cons}" ["l'"] ["p","l"]
         .| send "k_a" ["c","l'"]
          )
     .| recv "k_c" ["h","t"] .- caseof "c"
          [ "f" ~: new ["l'"]  -- store
              ( become "B_{cons}" ["l'"] ["p","l"]
             .| send "k_a" ["f","l'"]
              )
          , "x" ~: new ["l'"]  -- join
              ( send "k_P" ["p","h"]
             .| send "k_a" ["f","t"]
              )
          , "y" ~: new ["l'"]  -- join
              ( send "k_P" ["h","p"]
             .| send "k_a" ["f","t"]
              )
          ]
     .| recv "k_P" ["u","v"] .- become "P" [] []
     .| recv "k_a" ["f'","l'"] .- become "B_a^P" ["a"] ["x","y","f'","l'"]
      )

newenc =
  ( "newenc.tex"
  , encNew
  , [ encDefBc
    , encDefBnil
    , encDefBcons
    , encNewDefBa
    ]
  )
