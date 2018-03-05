forward(X, A) ->
  receive
    I -> join:send(A, {X, I}),
         forward(X, A)
  end.
