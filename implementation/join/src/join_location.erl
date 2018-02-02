-module(join_location).
-include("debug.hrl").
-compile(export_all).

% a location knows:
%   - its superlocation (by pid)
%   - its sublocations (by pid)
%   - its processes (by pid)
%   - TODO: own channel name?
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
% top location?
% joining the network?

% a location knows its subprocesses by Pid, since they cannot move without it
location(Name, Super, Subs, Actors) ->
  receive
    {register_process, Pid} ->
      location(Name, Super, Subs, [Pid | Actors]);
    {wrap_up, Pid, Ref} ->
      Ss = erlang:map(fun wrap_up/1, Subs), % TODO: concurrency
      As = erlang:map(fun join_actor:wrap_up/1, Actors),
      Super ! {unregister_location, self()},
      Pid ! {Ref, {Name, Ss, As}};
    status ->
      io:format("Super: ~p,~n", [Super]),
      io:format("Subs: ~p,~n", [Subs]),
      io:format("Actors: ~p~n", [Actors]),
      location(Name, Super, Subs, Actors);
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

respawn(Super, Data) ->
  % register at super location
  % register in gproc
  % register actors
  % register sub processes
  not_implemented.

create(Super, Name) ->
  Channel = join_util:get_id(Name),
  respawn(Super, {Channel, [], []}),
  Channel.

create_root() ->
  not_implemented.

