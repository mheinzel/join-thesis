-module(join_reg).
-include("debug.hrl").
-export([
        register_self/1,
        unregister_self/0,
        get_pid/1,
        get_pid/2,
        send/2
        ]).

% process registry.
% can be backed by either `gproc` or `global`.
%
% generally, gproc is the registry we want to use, since registration only
% happens at the local registry at first and is immediately available locally.
% later it is synced to the other nodes.
% its approach of allowing processes to only register themselves fits our use
% case well.
% also, it supports waiting for a name to be registered, which is useful to us.
%
% the problem with gproc is that there is a bug in the function for unregistering
% (goodbye/0).
% processes are only unregistered when they die.
% this makes it impossible to stay around for a while
% and forward everything to the newly registered process.
% it's also impossible to just overwrite the registration in the new process.
% currently, messages in the inbox will be lost when moving (when using gproc),
% but this could be fixed in the future.

% since gproc is currently not working, we have to use the `global` module
%-define(use_gproc, 1).


-ifdef(use_gproc).
% requires gproc application to be running

register_self(Channel) ->
  try gproc:reg({n, g, Channel})
  catch
    error:_ ->
      ?DEBUG("registering on ~p failed, retrying...p",
               [Channel]),
      timer:sleep(500),
      register_self_again(Channel)
  end.

register_self_again(Channel) ->
  try gproc:reg({n, g, Channel})
  catch
    error:Error ->
      % TODO: retry? rethrow?
      ?WARNING("registering on ~p failed with message:~n  ~p",
               [Channel, Error]),
      % maybe the channel is still registered somewhere else?
      try gproc:where({n, g, Channel}) of
        Pid ->
          ?WARNING("~p is registered at: ~p", [Channel, Pid])
      catch
        error:badarg ->
          ?WARNING("~p is probably not registered", [Channel])
      end
  end.

unregister_self() ->
  ?DEBUG("self() = ~p", [self()]),
  % seems to have no effect
  gproc:goodbye().

% waits for a registration to appear
% throws error:timeout
get_pid(Channel) ->
  get_pid(Channel, 8000).
get_pid(Channel, Timeout) ->
  {Pid, _} = gproc:await({n, g, Channel}, Timeout),
  Pid.


-else.

register_self(Channel) ->
  % overwrite in case the old one is still around
  case global:re_register_name(Channel, self()) of
    yes -> ok;
    % TODO: retry? throw?
    _ -> ?WARNING("registering on ~p failed~p", [Channel])
  end.

unregister_self() ->
  % needs channel name, but we can just not explicitly unregister for now,
  % since a new registration overwrites old ones.
  ok.

% no builtin waiting for registration, so we use an improvised retry mechanism
% throws error:timeout
get_pid(Channel) ->
  get_pid(Channel, [100, 500, 2000]).

% Timeout can be a single time or list of times (in ms) after which we retry
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

-endif.


% asynchronously looks up channel name and sends message
send(Channel, Payload) ->
  From = self(),
  spawn(fun() ->
            try get_pid(Channel) of
              Pid ->
                ?DEBUG("~p sending to ~p (~p):~n  ~p",
                       [From, Channel, Pid, Payload]),
                Pid ! Payload
            catch
              % just ignore, messages can be lost anyways
              error:timeout -> ?WARNING("failed sending to ~p:~n  ~p",
                                         [Channel, Payload])
            end
        end),
  ok.
