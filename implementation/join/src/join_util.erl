-module(join_util).
-export([
         create_id/1,
         indentation/1]).


% globally unique identifier
% uses a given tag to help with debugging
create_id(Who) ->
  {Who, node(), erlang:unique_integer([positive, monotonic])}.

% for pretty printing
indentation(Level) ->
  lists:append(lists:duplicate(Level, "|   ")).
