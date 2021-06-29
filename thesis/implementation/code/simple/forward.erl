forward(X, A) ->
  receive
    I -> join:send(A, {X, I}),
         forward(X, A)
  end.

join:spawn_actor(fun forward/2, Name, Destination).
