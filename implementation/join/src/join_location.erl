-module(join_location).
-include("debug.hrl").
-export([
         create_root/0,
         create/2,
         go/3,
         spawn_actor_at/4,
         print_status/1
        ]).
-export([forward_location/1]).

% a location knows:
%   - its own channel name
%   - its superlocation (by pid)
%   - its sublocations (by pid)
%   - its processes (by pid)
%
% knowing the pids is enough, since the tree will be rebuilt on migration anyways.
% so we can just store the new pids then.
%
% supported operations:
%   - spawning a process
%   - wrapping itself up into a data structure (ending execution)
%   - re-spawning such a data structure
%   - removing a sublocation (when it moves)
%   - (printing its status)
%
% it's not possible to remove a process, since it cannot move on its own.
% TODO: we can probably detect end of execution using somthing like a monitor.
%
% TODO:
% top location? (not movable, should be automatically created, managed and restarted)
% joining the network?


% TODO: leave forwarder behind after migrating?
location(Name, Super, Subs, Actors) ->
  receive
    {spawn_actor, Actor} ->
      Pid = join_actor:spawn_actor_here(Actor),
      location(Name, Super, Subs, [Pid | Actors]);
    {spawn_location, Data, Continuation} ->
      SubPid = respawn_here(Data),
      case Continuation of
        none -> ok;
        _ -> join_reg:send(Continuation, ok)
      end,
      location(Name, Super, [SubPid | Subs], Actors);
    {unregister_location, Pid} ->
      location(Name, Super, lists:delete(Pid, Subs), Actors);
    {wrap_up, Pid, Ref} ->
      case Super of
        none ->
          Pid ! {Ref, is_root}; % cannot migrate a root location
        _ ->
          Super ! {unregister_location, self()}, % TODO: sync?
          Ss = lists:map(fun wrap_up/1, Subs), % TODO: concurrency
          As = lists:map(fun join_actor:wrap_up/1, Actors),
          join_reg:unregister_self(),
          Pid ! {Ref, {ok, {Name, Ss, As}}}
          %forward_location(Name)
      end;
    {print_status, Pid, Ref, IndentLevel} ->
      io:format(join_util:indentation(IndentLevel) ++ "|-- location ~p (Pid: ~p, Super: ~p):~n",
                [Name, self(), Super]),
      lists:foreach(fun(A) -> print_status(A, IndentLevel+1) end, Actors),
      lists:foreach(fun(S) -> print_status(S, IndentLevel+1) end, Subs),
      Pid ! {Ref, ok},
      location(Name, Super, Subs, Actors);
    Other ->
      ?WARNING("location ~p received invalid message: ~p~n", [self(), Other]),
      location(Name, Super, Subs, Actors)
  end.

spawn_actor_at(Location, Actor, Channel, Args) ->
  join_reg:send(Location, {spawn_actor, {Actor, Channel, Args}}).


% synchronous
print_status(Loc) ->
  print_status(Loc, 0).

print_status(Loc, IndentLevel) when is_pid(Loc) ->
  Ref = make_ref(),
  Loc ! {print_status, self(), Ref, IndentLevel},
  receive
    {Ref, ok} -> ok
  after 500 ->
          io:format(join_util:indentation(IndentLevel) ++ "|- timeout~n")
  end;
print_status(Loc, IndentLevel) ->
  try join_reg:get_pid(Loc, 100) of
    Pid -> print_status(Pid, IndentLevel)
  catch
    error:timeout -> io:format(join_util:indentation(IndentLevel) ++ "|- timeout~n")
  end.


forward_location(Channel) ->
  timer:sleep(1000), % so we don't spam ourself

  try join_reg:get_pid(Channel, 200) of
    Pid when Pid=:=self() ->
      ?WARNING("(~p) still registered on ~p",
               [self(), Channel]),
      forward_location(Channel);
    _ ->
      % some-one else register, forward everything!
      forward_location_start(Channel)
  catch
    error:timeout ->
      ?WARNING("still no-one registered on ~p",
               [Channel]),
      forward_location(Channel)
  end.

forward_location_start(Channel) ->
  receive
    Message ->
      ?DEBUG("after moving to ~p:~n  ~p~n",
                [Channel, Message]),
      join_reg:send(Channel, Message),
      forward_location_start(Channel)
  after 5000 ->
          ?DEBUG("retiring...", []),
          ok
  end.



% Source might be ommited in the future
% Continuation can be none
go(Source, Destination, Continuation) ->
  spawn(fun() ->
            % race condition?
            try join_reg:get_pid(Source) of
              SourcePid ->
                Data = wrap_up(SourcePid),
                join_reg:send(Destination, {spawn_location, Data, Continuation})
            catch
              throw:timeout -> ?WARNING("~p cannot find location ~p to go to~n",
                                         [Source, Destination])
            end
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
          ?WARNING("timeout when wrapping up location at ~p~n", [Pid]),
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
            As = lists:map(fun(Actor) -> join_actor:spawn_actor_here(Actor) end, Actors),
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

