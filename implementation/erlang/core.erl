-module(core).
-export([
  def/2,
  actor/5,
  forward/2,
  test_print/0,
  test_pi_channel/0
]).

% TODO:
% parallel
% synchronous
% polyadic

% TODO: better solution for single channel def (no jon)
simple_handler(P) ->
  receive
    U -> P(self(), U), simple_handler(P)
  end.

def(P, Q) when is_function(Q, 1) ->
  X = spawn(fun() -> simple_handler(P) end),
  spawn(fun() -> Q(X) end);


def(P, Q) ->
  A = spawn(?MODULE, actor, [P, x, y, [], []]),
  X = spawn(?MODULE, forward, [x, A]),
  Y = spawn(?MODULE, forward, [y, A]),
  spawn(fun() -> Q(X, Y) end),
  A.

actor(P, X, Y, Us, Vs) ->
  receive
    {X, U} ->
      case Vs of
        [] -> actor(P, X, Y, [U | Us], []);
        [V | T] ->
          spawn(fun() -> P(X, U, Y, V) end),
          actor(P, X, Y, Us, T)
      end;
    {Y, V} ->
      case Us of
        [] -> actor(P, X, Y, [], [V | Vs]);
        [U | T] ->
          spawn(fun() -> P(X, U, Y, V) end),
          actor(P, X, Y, T, Vs)
      end;
    status ->
      io:format("Us: ~p~nVs: ~p~n", [Us, Vs]),
      actor(P, X, Y, Us, Vs)
  end.

forward(X, A) ->
  receive
    I -> A ! {X, I}, forward(X, A)
  end.


test_print() ->
  def(fun(_X,U,_Y,V) -> io:format("joined ~p and ~p~n", [U, V]) end,
      fun(X,Y) -> X ! "a", X ! "b", Y ! "1", Y ! "2", Y ! "3" end).

test_pi_channel() ->
  def(fun(_S,X,_R,K) -> K ! X end, % "reply X to _R"
      fun(Send,Receive) -> Send ! 1, Send ! 2,
                           def(fun(_K,V) -> io:format("received ~p~n", [V]) end,
                               fun(K) -> Receive ! K end) end).
