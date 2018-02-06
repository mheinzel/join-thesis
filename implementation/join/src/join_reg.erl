-module(join_reg).
-include("debug.hrl").
-export([
        register_self/1,
        unregister_self/0,
        get_pid/1,
        send/2
        ]).


-define(use_gproc, 1).

-ifdef(use_gproc).
% requires gproc application to be running

register_self(Channel) ->
  gproc:reg({n, g, Channel}).

unregister_self() ->
  gproc:goodbye().

% throws
get_pid(Channel) ->
  {Pid, _} = gproc:await({n, g, Channel}, 5000),  % might not be registered yet
  Pid.

send(Channel, Payload) ->
  ?DEBUG("sending ~p to ~p", [Payload, Channel]),
  spawn(fun() ->
            try join_reg:get_pid(Channel) of  % might not be registered yet
              Pid -> Pid ! Payload
            catch
              throw:timeout -> io:format("ERROR: failed sending ~p to ~p~n",
                                         [Payload, Channel])
            end
        end),
  ok.

-else.

register_self(Channel) ->
  global:register_name(Channel, self()).

unregister_self() ->
  ok.
  % needs channel name
  % global:unregister_name(Channel).

% does not wait, which currently leads to errors
get_pid(Channel) ->
  global:whereis_name(Channel).

send(Channel, Payload) ->
  global:send(Channel, Payload).

-endif.
