join:def(fun(X, Y) -> {
  fun(U, V) ->
    io:format("joined ~p and ~p~n", [U, V])
  end,
  fun() ->
    join:send(X, "a"),
    join:send(Y, 1),
    join:send(Y, 2),
  end
} end).
