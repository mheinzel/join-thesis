-module(join).
-include("debug.hrl").
-export([
         send/2,
         def/2
        ]).

% re-export
send(Channel, Payload) ->
  join_reg:send(Channel, Payload).

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
  join_location:spawn_actor_at(Location, join_actor:definition(P), A, [X, Y, Lx, Ly]),  % P passed at runtime only in implementation
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, Y, [A]),
  spawn(Q),
  % return actor name just for debugging
  A.

