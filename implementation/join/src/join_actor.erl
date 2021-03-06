-module(join_actor).
-include("debug.hrl").
-export([
         definition/1,
         forward/2,
         spawn_actor_here/1,
         wrap_up/1
         ]).

% an actor is a process that is receiving messages and is registered under
% a channel name in the registry and at a location.
% the concept is based on a behavior in the actor pi calculus.
%
% generally, it knows:
%   - its own channel name
%   - some arguments (usually to keep state)
%
% it can be represented and sent in the form:
%   {Actor, Channel, Args}
% where
%   Actor is a function taking the two arguments Channel and Args.
%
% supported operations:
%   - wrapping itself up into a data structure (ending execution)
%   - (printing its status)
%
% actors can be spawned at a location (see join_location:spawn_actor_at/4),
% which will automatically register them there and in the global registry.

% spawn an actor (as data) and register it in the registry.
% returns pid of spawned process.
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
%   - the two channels it joins
%   - the payloads of received, but unjoined messages on these channels
%
% corresponds to B_a in the encoding

definition(P) ->
  fun Actor(A, X, Y, Flag, Payloads) ->
    receive
      {Channel, Payload} ->
        case queue:out(Payloads) of
          {empty, _} ->
            Actor(A, X, Y, Channel, queue:in(Payload, Payloads));
          {{value, H}, T} ->
            case Flag of
              X ->
                case Channel of
                  X ->
                    Actor(A, X, Y, Flag, queue:in(Payload, Payloads));
                  Y ->
                    spawn(fun() -> P(H, Payload) end),
                    Actor(A, X, Y, Flag, T)
                end;
              Y ->
                case Channel of
                  Y ->
                    Actor(A, X, Y, Flag, queue:in(Payload, Payloads));
                  X ->
                    spawn(fun() -> P(Payload, H) end),
                    Actor(A, X, Y, Flag, T)
                end
            end
        end;

      {wrap_up, Pid, Ref} ->
        % unregister and send itself back as data
        join_reg:unregister_self(),
        Pid ! {Ref, {Actor, A, [X, Y, Flag, Payloads]}},
        % forward all messages to the new location
        join_forward:forward_on(A);

      {print_status, Pid, Ref, Level} ->
        io:format(join_util:indentation(Level+1) ++ "definition ~p (Pid: ~p):~n",
                  [A, self()]),
        io:format(join_util:indentation(Level+1) ++ "  Flag: ~p~n", [Flag]),
        io:format(join_util:indentation(Level+1) ++ "  Payloads: ~p~n", [Payloads]),
        Pid ! {Ref, ok},
        Actor(A, X, Y, Flag, Payloads);

      Other ->
        ?WARNING("definition ~p received invalid message: ~p~n",
                 [self(), Other]),
        Actor(A, X, Y, Flag, Payloads)
    end
  end.


% a forward actor additionally knows:
%   - the channel it has to forward to
%
% corresponds to B_c in the encoding.

forward(X, A) ->
  receive
    {wrap_up, Pid, Ref} ->
      % unregister and send itself back as data
      join_reg:unregister_self(),
      Pid ! {Ref, {fun forward/2, X, [A]}},
      % forward all messages to the new location
      join_forward:forward_on(X);

    {print_status, Pid, Ref, IndentLevel} ->
      io:format(join_util:indentation(IndentLevel+1) ++ "forward ~p to ~p~n",
                [X, A]),
      Pid ! {Ref, ok},
      forward(X, A);

    I -> join_reg:send(A, {X, I}),
         forward(X, A)
  end.

