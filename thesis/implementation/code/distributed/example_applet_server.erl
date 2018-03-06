def_single_globally(Location, cell, fun(Cell) -> {
  fun({A, ContCell}) ->
    io:format("applet requested~n"),
    join:def_location(Location, applet, fun(Applet, Get, Put) -> {
      fun(K, X) ->
        io:format("got ~p from cell~n", [X]),
        join:send(K, X)
      end,
      fun() ->
        io:format("moving applet to ~p~n", [A]),
        join:go(Applet, A, none)
      end,
      fun() ->
        io:format("sending get, put refs to ~p~n", [ContCell]),
        join:send(ContCell, {Get, Put})
      end
    } end)
  end,
  fun() ->
    ok
  end
} end).
