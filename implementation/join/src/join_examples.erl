-module(join_examples).
-compile(export_all).


print() ->
  join:def(fun(_X,U,_Y,V) -> io:format("joined ~p and ~p~n", [U, V]) end,
      fun(X,Y) -> X ! "a", X ! "b", Y ! "1", Y ! "2", Y ! "3" end).

pi_channel() ->
  join:def(fun(_S,X,_R,K) -> K ! X end, % "reply X to _R"
      fun(Send,Receive) -> Send ! 1, Send ! 2,
                           join:def(fun(_K,V) -> io:format("received ~p~n", [V]) end,
                               fun(K) -> Receive ! K end) end).
