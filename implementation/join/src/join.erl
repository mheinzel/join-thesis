-module(join).
-compile(export_all).


-define(DEBUG(F, A), ok).
%-define(DEBUG(Format, Args), io:format("~p:~p: " ++ Format ++ "~n", [?FUNCTION_NAME, ?LINE | Args])).

% TODO
% split out insert and join?
% rename function names corresponing to actor definitions?



% node() makes it globally unique
get_id(Who) ->
  {Who, node(), erlang:unique_integer()}.

send(Channel, Payload) ->
  ?DEBUG("sending ~p to ~p", [Payload, Channel]),
  spawn(fun() ->
            {Pid, _} = gproc:await({n, l, Channel}),  % might not be registered yet
            Pid ! Payload
        end).

% spawn and register a behavior on a given channel
% based on behavior syntax in ActorPi
behavior(Bhv, Channel, Args) ->
  ?DEBUG("starting on ~p", [Channel]),
  spawn(fun() ->
            gproc:reg({n, l, Channel}),
            apply(Bhv, [Channel | Args])
        end).


% temporary, just to make it easier to define continuations
% (could also be overloaded with regular def)
def_single(PQ) ->
  A = get_id(sng),
  {P, Q} = PQ(A),
  Actor = fun Single(_) ->
      receive
        U -> spawn(fun() -> P(U) end),
             Single
      end
    end,
  behavior(Actor, A, []),
  spawn(Q),
  A.

def(PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = get_id(act),
  X = get_id(fwX),
  Y = get_id(fwY),
  Lx = queue:new(),
  Ly = queue:new(),
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  behavior(actor(P), A, [X, Y, Lx, Ly]),  % P passed at runtime only in implementation
  behavior(fun forward/2, X, [A]),
  behavior(fun forward/2, Y, [A]),
  spawn(Q),
  % return actor name just for debugging
  A.


% B_a
actor(P) ->
  fun Actor(A, X, Y, Us, Vs) ->
    receive
      {X, U} ->
        case queue:out(Vs) of
          {empty, _} -> Actor(A, X, Y, queue:in(U, Us), Vs);
          {{value, V}, T} ->
            spawn(fun() -> P(U, V) end),
            Actor(A, X, Y, Us, T)
        end;
      {Y, V} ->
        case queue:out(Us) of
          {empty, _} -> Actor(A, X, Y, Us, queue:in(V, Vs));
          {{value, U}, T} ->
            spawn(fun() -> P(U, V) end),
            Actor(A, X, Y, T, Vs)
        end;
      status ->
        io:format("Us: ~p~nVs: ~p~n", [Us, Vs]),
        Actor(A, X, Y, Us, Vs);
      Other ->
        io:format("warning: actor received invalid message: ~p~n", [Other]),
        Actor(A, X, Y, Us, Vs)
    end
  end.

% B_{c}
forward(X, A) ->
  receive
    I -> send(A, {X, I}),
         forward(X, A)
  end.
