% takes an additional location identifier (just for debugging).
% PQ now also receives a location name (as first parameter).
def_location(Location, NewLocName, PRQ) ->
  % new names
  A = join_util:create_id(act),
  X = join_util:create_id(fwX),
  Y = join_util:create_id(fwY),
  Payloads = queue:new(),
  % create location
  NewLocation = join_location:create(Location, NewLocName),
  % get user-supplied processes
  {P, R, Q} = PRQ(NewLocation, X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(NewLocation, join_actor:definition(P),
                               A, [X, Y, X, Payloads]),
  join_location:spawn_actor_at(NewLocation,
                               fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(NewLocation,
                               fun join_actor:forward/2, Y, [A]),
  spawn(R),
  spawn(Q).
