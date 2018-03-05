def(PQ) ->
  % new names
  A = join:create_id(act),
  X = join:create_id(fwX),
  Y = join:create_id(fwY),
  Payloads = [],
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  join:spawn_actor(join_actor:definition(P), A, [X, Y, X, Payloads]),
  join:spawn_actor(fun join_actor:forward/2, X, [A]),
  join:spawn_actor(fun join_actor:forward/2, Y, [A]),
  spawn(Q).
