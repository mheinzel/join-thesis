-module(join).
-include("debug.hrl").
-compile(export_all).



% TODO
% rename function names corresponing to actor definitions?
% top location should always exist (and be defaulted to?)



send(Channel, Payload) ->
  ?DEBUG("sending ~p to ~p", [Payload, Channel]),
  spawn(fun() ->
            {Pid, _} = gproc:await({n, l, Channel}),  % might not be registered yet
            Pid ! Payload
        end).




def(Location, PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = join_util:get_id(act),
  X = join_util:get_id(fwX),
  Y = join_util:get_id(fwY),
  Lx = queue:new(),
  Ly = queue:new(),
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  join_actor:spawn_at(Location, actor(P), A, [X, Y, Lx, Ly]),  % P passed at runtime only in implementation
  join_actor:spawn_at(Location, fun forward/2, X, [A]),
  join_actor:spawn_at(Location, fun forward/2, Y, [A]),
  spawn(Q),
  % return actor name just for debugging
  A.


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



% TODO: split to location and actor
wrap_up(Pid) ->
  Ref = make_ref(),
  Pid ! {wrap_up, self(), Ref},
  receive
    {Ref, ProcData} -> ProcData
  after 3000 -> timeout
  end.
