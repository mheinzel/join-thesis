-module(join).
-include("debug.hrl").
-export([
         send/2,
         go/3,
         def/2,
         def_location/3,
         def_globally/4
        ]).

% re-export
send(Channel, Payload) ->
  join_reg:send(Channel, Payload).

% must use the current (innermost) location as source!!!
go(Source, Destination, Continuation) ->
  join_location:go(Source, Destination, Continuation).



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

def_location(Location, NewLocName, PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = join_util:get_id(act),
  X = join_util:get_id(fwX),
  Y = join_util:get_id(fwY),
  Lx = queue:new(),
  Ly = queue:new(),
  % create location
  NewLocation = join_location:create(Location, NewLocName),
  % get user-supplied processes
  % def NewLocation [ X | Y > P  :  R] in Q
  {P, R, Q} = PQ(NewLocation, X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(NewLocation, join_actor:definition(P), A, [X, Y, Lx, Ly]),  % P passed at runtime only in implementation
  join_location:spawn_actor_at(NewLocation, fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(NewLocation, fun join_actor:forward/2, Y, [A]),
  % TODO: spawn at the new location? should be on this node actually, so doesn't matter
  spawn(R),
  spawn(Q),
  % return actor name just for debugging
  A.


% global names have to be globally unique
% TODO: should they always be registered at root location?
def_globally(Location, X, Y, PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = join_util:get_id(act),
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
