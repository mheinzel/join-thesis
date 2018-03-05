forward(X, A) ->
  receive
    I -> join_reg:send(A, {X, I}),
         forward(X, A)
  end.
