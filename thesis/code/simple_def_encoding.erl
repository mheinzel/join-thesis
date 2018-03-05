def(Location, PQ) ->
  % new names
  A = join_util:create_id(act),
  X = join_util:create_id(fwX),
  Y = join_util:create_id(fwY),
  Payloads = [],
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(Location, join_actor:definition(P),
                               A, [X, Y, X, Payloads]),
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, Y, [A]),
  spawn(Q).
