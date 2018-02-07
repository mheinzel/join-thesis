-module(join_actor).
-include("debug.hrl").
-export([
         definition/1,
         forward/2,
         spawn_actor_here/1,
         wrap_up/1
         ]).

% a general actor is process registered at a location and knows:
%   - TODO: its location? forward doesn't need it.
%   - its own channel name
%   - some arguments (usually to keep state)
%
% supported operations:
%   - wrapping itself up into a data structure (ending execution)
%   - (printing its status)
%
% it is based on the concept of a behavior in the actor pi calculus.
%
% actors can be spawned at a location (see join_location.erl), TODO: move back here?
% which will automatically register them there and in the global registry.

% returns Pid
spawn_actor_here({Actor, Channel, Args}) ->
  ?DEBUG("spawning actor on ~p", [Channel]),
  spawn(fun() ->
            join_reg:register_self(Channel),
            apply(Actor, [Channel | Args])
        end).

% almost as for locations, but actors don't return errors
wrap_up(Pid) ->
  Ref = make_ref(),
  Pid ! {wrap_up, self(), Ref},
  receive
    {Ref, Data} -> Data
   after 3000 -> timeout
  end.

% a definition actor is parametrized over a process P and additionally knows:
%   - the two channels it needs to join
%   - the payloads of received, but unjoined messages on these channels
%

% TODO:
% after wrapping up, stay as a forwarder for a while
% B_a
definition(P) ->
  fun Actor(A, X, Y, Us, Vs) ->
    receive
      {wrap_up, Pid, Ref} ->
        join_reg:unregister_self(),
        Pid ! {Ref, {Actor, A, [X, Y, Us, Vs]}},
        join_location:forward_location(A);
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
      {print_status, Pid, Ref, IndentLevel} ->
        io:format(join_util:indentation(IndentLevel+1) ++ "definition ~p (Pid: ~p):~n", [A, self()]),
        io:format(join_util:indentation(IndentLevel+1) ++ "  Us: ~p~n", [Us]),
        io:format(join_util:indentation(IndentLevel+1) ++ "  Vs: ~p~n", [Vs]),
        Pid ! {Ref, ok},
        Actor(A, X, Y, Us, Vs);
      Other ->
        ?WARNING("definition received invalid message: ~p~n", [Other]),
        Actor(A, X, Y, Us, Vs)
    end
  end.


% a forward actor ... TODO

% B_{c}
forward(X, A) ->
  receive
    {wrap_up, Pid, Ref} ->
      join_reg:unregister_self(),
      Pid ! {Ref, {fun forward/2, X, [A]}},
      join_location:forward_location(X);
    {print_status, Pid, Ref, IndentLevel} ->
      io:format(join_util:indentation(IndentLevel+1) ++ "forward ~p to ~p~n",
                [X, A]),
      Pid ! {Ref, ok},
      forward(X, A);
    I -> join_reg:send(A, {X, I}),
         forward(X, A)
  end.

