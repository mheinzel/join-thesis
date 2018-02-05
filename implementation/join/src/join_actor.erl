-module(join_actor).
-include("debug.hrl").
-compile(export_all).

% a process needs to do some things when it is spawned:
%   - register itself in gproc
%   - register itself in the location

% spawn and register a behavior on a given channel, in a given location
% based on behavior syntax in ActorPi
spawn_at(Location, Bhv, Channel, Args) ->
  ?DEBUG("at ~p", [Location]),
  % TODO: synchronously, so we're not forgotten?
  join_location:register_self(Location),
  join_actor:spawn(Bhv, Channel, Args).

spawn(Bhv, Channel, Args) ->
  ?DEBUG("starting ~p", [Channel]),
  spawn(fun() ->
            gproc:reg({n, l, Channel}),
            apply(Bhv, [Channel | Args])
        end).


% wrap_up(Pid) ->
%   Ref = make_ref(),
%   Pid ! {wrap_up, self(), Ref},
%   receive
%     {Ref, ProcData} -> ProcData
%   after 3000 -> timeout
%   end.
%


send(Channel, Payload) ->
  ?DEBUG("sending ~p to ~p", [Payload, Channel]),
  spawn(fun() ->
            Pid = get_pid(Channel),  % might not be registered yet
            Pid ! Payload
        end).

get_pid(Channel) ->
  {Pid, _} = gproc:await({n, l, Channel}),  % might not be registered yet
  Pid.



% TODO:
% after wrapping up, stay as a forwarder for a while
% B_a
actor(P) ->
  fun Actor(A, X, Y, Us, Vs) ->
    receive
      {wrap_up, Pid, Ref} ->  % Pid or Channel?
        Pid ! {Ref, {Actor, A, [X, Y, Us, Vs]}};
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
    {wrap_up, Pid, Ref} ->
      Pid ! {Ref, {forward, X, [A]}};
    I -> send(A, {X, I}),
         forward(X, A)
  end.
