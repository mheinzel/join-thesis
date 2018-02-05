-module(join_location).
-include("debug.hrl").
-compile(export_all).

% a location knows:
%   - its superlocation (by pid)
%   - its sublocations (by pid)
%   - its processes (by pid)
%   - its own channel name
%
% knowing the pids is enough, since the tree will be rebuilt on migration anyways.
% so we can just store the new pids then.
%
% supported operations:
%   - wrapping it up into a data structure (ending execution)
%   - re-spawning such a data structure
%   - adding a sublocation
%   - removing a sublocation (when it moves)
%   - adding a process
%   - (printing its status)
%
% it's not possible to remove a process, since it cannot move on its own.
% TODO: we can probably detect end of execution using somthing like a monitor.
%
% TODO:
% top location? (should be automatically created, managed and restarted)
% joining the network?


location(Channel, Super, Subs, Actors) ->
  receive
    {register_process, Pid} ->
      location(Channel, Super, Subs, [Pid | Actors]);
    {register_location, Pid} ->
      location(Channel, Super, [Pid | Subs], Actors);
    {unregister_location, Pid} ->
      location(Channel, Super, lists:delete(Pid, Subs), Actors);
    {wrap_up, Pid, Ref} ->
      Ss = lists:map(fun wrap_up/1, Subs), % TODO: concurrency
      As = lists:map(fun join_actor:wrap_up/1, Actors),
      % only non-root locations need to unregister at superlocation
      case Super of
        none -> ok;
        _ -> Super ! {unregister_location, self()}
      end,
      Pid ! {Ref, {Channel, Ss, As}};
    status ->
      io:format("Super: ~p,~n", [Super]),
      io:format("Subs: ~p,~n", [Subs]),
      io:format("Actors: ~p~n", [Actors]),
      location(Channel, Super, Subs, Actors);
    Other ->
      io:format("location ~p received invalid message: ~p~n", [self(), Other])
  end.

wrap_up(Pid) ->
  Ref = make_ref(),
  Pid ! {wrap_up, self(), Ref},
  receive
    {Ref, Data} -> Data
  after 3000 -> timeout
  end.

% race condition?
% sublocations and actors need to be registered before receive messages
respawn(Super, {Channel, Subs, Actors}) ->
  spawn(fun() ->
            OwnPid = self(),
            % register at super location
            join:send(Super, {register_location, OwnPid}),
            % register in gproc
            gproc:reg({n, l, Channel}),
            % register actors
            lists:foreach(fun({Actor, A, Args}) -> join_actor:spawn_at(Channel, Actor, A, Args) end, Actors),
            % register sub processes
            lists:foreach(fun(Sub) -> respawn(OwnPid, Sub) end, Subs),
            location(Channel, Super
        end).

create(Super, Name) ->
  Channel = join_util:get_id(Name),
  respawn(Super, {Channel, [], []}),
  Channel.

create_root() ->
  Channel = {root, node()},
  spawn(fun() ->
            gproc:reg({n, l, Channel}),
            location(Channel, none, [], [])
        end),
  Channel.

