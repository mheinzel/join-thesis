-module(join).
-export([
  def/2,
  actor/5,
  forward/2
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
  A = spawn(?MODULE, actor, [P, x, y, queue:new(), queue:new()]),
  X = spawn(?MODULE, forward, [x, A]),
  Y = spawn(?MODULE, forward, [y, A]),
  spawn(fun() -> Q(X, Y) end),
  A.

actor(P, X, Y, Us, Vs) ->
  receive
    {X, U} ->
      case queue:out(Vs) of
        {empty, _} -> actor(P, X, Y, queue:in(U, Us), Vs);
        {{value, V}, T} ->
          spawn(fun() -> P(X, U, Y, V) end),
          actor(P, X, Y, Us, T)
      end;
    {Y, V} ->
      case queue:out(Us) of
        {empty, _} -> actor(P, X, Y, Us, [V | Vs]);
        {{value, U}, T} ->
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
