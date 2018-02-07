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

% just to make it easier to define continuations
def_single_globally(Location, GlobalName, PQ) ->
  UnusedName = join_util:get_id(unused),
  join:def_globally(Location, GlobalName, UnusedName, fun(X, Unused) ->
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

beeper(L) ->
  def_single(L, fun(Token) -> {
                        fun(_) ->
                            io:format("beep!~n"),
                            timer:sleep(2000),
                            join:send(Token, ok)
                        end,
                        fun() ->
                            join:send(Token, ok)
                        end
                       }
              end).

% simplified applet example
applet_server(Location) ->
  def_single_globally(Location, cell, fun(_Cell) -> {
    fun({A, ContCell}) ->

      join:def_location(Location, applet, fun(Applet, Get, Put) -> {
        fun(K, X) ->
          io:format("got ~p from cell~n", [X]),
          join:send(K, X)
        end,
        % :
        fun() ->
          join:go(Applet, A, none) % can receive messages before migrating
        end,
        % in
        fun() ->
          % reply Get, Put to Cell
          join:send(ContCell, {Get, Put})
        end
      } end)

    end,
    %in
    fun() ->
      % cell already registered on nameserver
      ok
    end
  } end).

applet_client(User, NumGet, ToPrint) ->

  % get, put = cell(User) in
  % put("world");
  % put("hello");
  % print(get());
  % print(get());
  def_single(User, fun(Cont) -> {
    fun({Get, Put}) ->

      def_single(User, fun(Print) -> {
        fun(X) ->
          io:format("~p~n", [X])
        end,
        % in
        fun() ->
          lists:foreach(fun(X) -> join:send(Put, X) end, ToPrint),
          lists:foreach(fun(K) -> join:send(Get, K) end, lists:duplicate(NumGet, Print))
        end
      } end)

    end,
    % in
    fun() ->
      join:send(cell, {User, Cont})
    end
  } end).
