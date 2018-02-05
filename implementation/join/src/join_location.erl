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
% top location? (not movable, should be automatically created, managed and restarted)
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
      case Super of
        none ->
          Pid ! {Ref, is_root}; % cannot migrate a root location
        _ ->
          Super ! {unregister_location, self()},
          Ss = lists:map(fun wrap_up/1, Subs), % TODO: concurrency
          As = lists:map(fun join_actor:wrap_up/1, Actors),
          Pid ! {Ref, {ok, {Channel, Ss, As}}}
      end;
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
    {Ref, {ok, Data}} ->
      Data;
    {Ref, Error} ->
      io:format("could not wrap up location at ~p: ~p~n", [Pid, Error]),
      {error, Error}
  after 3000 ->
          io:format("timeout when wrapping up location at ~p~n", [Pid]),
          timeout
  end.

respawn_at(Super, Data) ->
  % TODO: synchronously?
  NewPid = respawn(Super, Data),
  Super ! {register_location, NewPid}.

% does not register at a superlocation!
respawn(Super, {Channel, Subs, Actors}) ->
  spawn(fun() ->
            % register in gproc
            gproc:reg({n, l, Channel}),
            % register actors
            As = lists:map(fun({Actor, A, Args}) -> join_actor:spawn(Channel, Actor, A, Args) end, Actors),
            % register sub processes
            Ss = lists:map(fun(Sub) -> respawn(self(), Sub) end, Subs),
            location(Channel, Super, Ss, As)
        end).

create(Super, Name) ->
  Channel = join_util:get_id(Name),
  respawn_at(Super, {Channel, [], []}),
  Channel.

create_root() ->
  Channel = {root, node()},
  respawn(none, {Channel, [], []}),
  Channel.

