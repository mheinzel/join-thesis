def(PQ) ->
  A = join:create_id(act),
  X = join:create_id(fwX),
  Y = join:create_id(fwY),
  Payloads = [],

  {P, Q} = PQ(X, Y),

  join:spawn_actor(join:definition(P),
                   A, [X, Y, X, Payloads]),
  join:spawn_actor(fun join:forward/2,
                   X, [A]),
  join:spawn_actor(fun join:forward/2,
                   Y, [A]),
  spawn(Q).
