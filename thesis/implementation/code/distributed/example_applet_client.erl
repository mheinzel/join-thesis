def_single(Location, fun(Cont) -> {
  fun({Get, Put}) ->
    def_single(Location, fun(Print) -> {
      fun(X) ->
        io:format("~p~n", [X])
      end,
      fun() ->
        join:send(Put, hello),
        join:send(Put, world),
        join:send(Get, Print),
        join:send(Get, Print)
      end
    } end)
  end,
  fun() ->
    join:send(cell, {Location, Cont})
  end
} end).
