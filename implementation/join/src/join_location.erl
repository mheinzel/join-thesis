-module(join_location).
-include("debug.hrl").
-export([
         create_root/0,
         create/2,
         go/3,
         print_status/1
        ]).

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


% TODO: leave forwared behind after migrating?
location(Channel, Super, Subs, Actors) ->
  receive
    {register_process, Pid} ->
      location(Channel, Super, Subs, [Pid | Actors]);
    % TODO: remove?
    {register_location, Pid} ->
      location(Channel, Super, [Pid | Subs], Actors);
    {spawn_location, Data, Continuation} ->
      SubPid = respawn_here(Data),
      case Continuation of
        none -> ok;
        _ -> join_reg:send(Continuation, ok)
      end,
      location(Channel, Super, [SubPid | Subs], Actors);
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
          % TODO: use unregister_self() ?
      end;
    {print_status, Pid, Ref, IndentLevel} ->
      io:format(indent(IndentLevel) ++ "|-- location ~p (Super: ~p, ~p actors):~n",
                [Channel, Super, length(Actors)]),
      lists:foreach(fun(S) -> print_status(S, IndentLevel+1) end, Subs),
      Pid ! {Ref, ok},
      location(Channel, Super, Subs, Actors);
    Other ->
      io:format("location ~p received invalid message: ~p~n", [self(), Other])
  end.

indent(Level) ->
  lists:append(lists:duplicate(Level, "    ")).

% synchronous
print_status(Loc) ->
  print_status(Loc, 0).

print_status(Loc, IndentLevel) when is_pid(Loc) ->
  Ref = make_ref(),
  Loc ! {print_status, self(), Ref, IndentLevel},
  receive
    {Ref, ok} -> ok
  end;
print_status(Loc, IndentLevel) ->
  try join_reg:get_pid(Loc) of
    Pid -> print_status(Pid, IndentLevel)
  catch
    throw:timeout -> io:format(indent(IndentLevel) ++ "|- timeout~n")
  end.




% Source might be ommited in the future
% Continuation can be none
go(Source, Destination, Continuation) ->
  spawn(fun() ->
            SourcePid = join_reg:get_pid(Source),
            Data = wrap_up(SourcePid),
            join_reg:send(Destination, {spawn_location, Data, Continuation})
        end).


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

% does not register at a superlocation!
respawn_here(Data) ->
  Super = self(),
  respawn_at(Super, Data).

respawn_at(Super, {Channel, Subs, Actors}) ->
  spawn(fun() ->
            % register in registry
            join_reg:register_self(Channel),
            % register actors
            As = lists:map(fun({Actor, A, Args}) -> join_reg:spawn(Channel, Actor, A, Args) end, Actors),
            % register sub processes
            Ss = lists:map(fun(Sub) -> respawn_here(Sub) end, Subs),
            location(Channel, Super, Ss, As)
        end).


create(Super, Name) ->
  Channel = join_util:get_id({loc, Name}),
  Data = {Channel, [], []},
  join_reg:send(Super, {spawn_location, Data, none}),
  Channel.

create_root() ->
  Channel = {root, node()},
  respawn_at(none, {Channel, [], []}),
  Channel.
