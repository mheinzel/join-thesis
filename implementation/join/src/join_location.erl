-module(join_location).
-include("debug.hrl").
-export([]).

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

% a location knows its subprocesses by Pid, since they cannot move without it
location(Super, Subs, Actors) ->
  receive
    {register_process, Pid} ->
      location(Super, Subs, [Pid | Actors]);
    {wrap_up, Pid, Ref} ->
      Ss = erlang:map(fun wrap_up/1, Subs), % TODO: concurrency
      As = erlang:map(fun join_actor:wrap_up/1, Actors),
      Super ! {unregister_location, self()},
      Pid ! {Ref, Ss, As};
    status ->
      io:format("Super: ~p,~n", [Super]),
      io:format("Subs: ~p,~n", [Subs]),
      io:format("Actors: ~p~n", [Actors]),
      location(Super, Subs, Actors);
    Other ->
      io:format("location ~p received invalid message: ~p~n", [self(), Other])
  end.

wrap_up(Pid) ->
  Ref = make_ref(),
  receive
    {Ref, Data} -> Data
  after 3000 -> timeout
  end.

respawn(Super, Data) -> not_implemented.

create(Super, Name) ->
  Channel = join_util:get_id(Name),
  respawn(Super, {Name, [], []}),
  Channel.
