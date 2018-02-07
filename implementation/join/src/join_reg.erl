-module(join_reg).
-include("debug.hrl").
-export([
        register_self/1,
        unregister_self/0,
        get_pid/1,
        get_pid/2,
        send/2
        ]).


-define(use_gproc, 1).

-ifdef(use_gproc).
% requires gproc application to be running

register_self(Channel) ->
  try gproc:reg({n, g, Channel})
  catch
    error:Error ->
      ?WARNING("registering on ~p failed with message:~n  ~p", [Channel, Error]),
      ?WARNING("~p is registered at: ~p", [gproc:where({n, g, Channel})])
  end.

unregister_self() ->
  ?DEBUG("self() = ~p", [self()]),
  gproc:goodbye().

% throws
get_pid(Channel) ->
  get_pid(Channel, 8000).
get_pid(Channel, Timeout) ->
  {Pid, _} = gproc:await({n, g, Channel}, Timeout),  % might not be registered yet
  Pid.

send(Channel, Payload) ->
  ?DEBUG("sending ~p to ~p", [Payload, Channel]),
  spawn(fun() ->
            try join_reg:get_pid(Channel) of  % might not be registered yet
              Pid -> Pid ! Payload
            catch
              error:timeout -> ?WARNING("failed sending to ~p:~n  ~p",
                                         [Channel, Payload])
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
