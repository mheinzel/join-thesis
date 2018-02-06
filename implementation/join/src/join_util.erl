-module(join_util).
-export([
         get_id/1,
         indentation/1]).


% node() makes it globally unique
get_id(Who) ->
  {Who, node(), erlang:unique_integer()}.


indentation(Level) ->
  lists:append(lists:duplicate(Level, "    ")).
