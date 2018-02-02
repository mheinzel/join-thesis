-module(join_examples).
-compile(export_all).


print(L) ->
  join:def(L, fun(X, Y) -> {
    fun(U, V) ->
      io:format("joined ~p and ~p~n", [U, V])
    end,
    % in
    fun() ->
      join:send(X, "a"),
      join:send(X, "b"),
      join:send(Y, "1"),
      join:send(Y, "2"),
      join:send(Y, "3")
    end
  } end).



% just to make it easier to define continuations
def_single(Location, PQ) ->
  join:def(Location, fun(X, Unused) ->
    {P, Q} = PQ(X),
    { fun(U, _) -> join:send(Unused, unused), P(U) end,
      fun() -> join:send(Unused, unused), Q() end}
  end).

pi_channel(L) ->
  join:def(L, fun(Send, Receive) -> {
    fun(X, K) ->
      join:send(K, X)
    end,
    % in
    fun() ->
      join:send(Send, 1),
      join:send(Send, 2),
      def_single(L, fun(K) -> {
        fun(V) -> io:format("received ~p~n", [V]) end,
        % in
        fun() -> join:send(Receive, K) end
      } end)
    end
  } end).
