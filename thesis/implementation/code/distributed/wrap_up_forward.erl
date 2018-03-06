forward(X, A) ->
  receive
    {wrap_up, Pid, Ref} ->
      % unregister and send itself back as data
      join_reg:unregister_self(),
      Pid ! {Ref, {fun forward/2, X, [A]}},
      % forward all messages to the new location
      join_forward:forward_on(X);

    I -> join_reg:send(A, {X, I}),
         forward(X, A)
  end.
