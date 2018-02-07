-module(join_reg).
-include("debug.hrl").
-export([
        register_self/1,
        unregister_self/0,
        get_pid/1,
        get_pid/2,
        send/2
        ]).


% gproc not working
%-define(use_gproc, 1).

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


-else.

register_self(Channel) ->
  % overwrite in case the old one is still around
  case global:re_register_name(Channel, self()) of
    yes -> ok;
    _ -> ?WARNING("registering on ~p failed~p", [Channel])
  end.

unregister_self() ->
  ok.
  % needs channel name, so just don't explicitly unregister for now
  % global:unregister_name(Channel).

% does not wait, which currently leads to errors
% TODO
get_pid(Channel, Timeout) when is_number(Timeout) ->
  get_pid(Channel, [Timeout]);
get_pid(Channel, []) ->
  case global:whereis_name(Channel) of
    undefined ->
      erlang:error(timeout);
    Pid ->
      Pid
  end;
get_pid(Channel, [Timeout | Rest]) ->
  case global:whereis_name(Channel) of
    undefined ->
      ?DEBUG("name ~p not found, retrying in ~p ms", [Channel, Timeout]),
      timer:sleep(Timeout),
      get_pid(Channel, Rest);
    Pid -> Pid
  end.

get_pid(Channel) ->
  get_pid(Channel, [100, 500, 2000]).

-endif.


send(Channel, Payload) ->
  From = self(),
  spawn(fun() ->
            try get_pid(Channel) of  % might not be registered yet
              Pid ->
                ?DEBUG("~p sending to ~p (~p):~n  ~p", [From, Channel, Pid, Payload]),
                Pid ! Payload
            catch
              error:timeout -> ?WARNING("failed sending to ~p:~n  ~p",
                                         [Channel, Payload])
            end
        end),
  ok.
