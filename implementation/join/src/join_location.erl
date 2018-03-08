-module(join_location).
-include("debug.hrl").
-export([
         create_root/0,
         root_of/1,
         create/2,
         go/3,
         spawn_actor_at/4
        ]).

% a location knows:
%   - its own channel name
%   - its superlocation (by pid)
%   - its sublocations (by pid)
%   - its actors (by pid)
%
% knowing pids is enough, since the tree will be rebuilt on migration anyways.
% we can just store the new pids then.
%
% it can be represented and sent in the form:
%   {Name, SubLocations, Actors}
% where
%   Name is the channel name,
%   SubLocations is a list of location data,
%   Actors is a list of actor data.
%
% supported operations:
%   - spawning a process
%   - wrapping itself up into a data structure (ending execution)
%   - re-spawning such a data structure
%   - removing a sublocation (when it moves)
%   - (printing its status)
%
% it's not possible to remove a spawned process (not actor),
% since it cannot move on its own.

location(Name, Super, Subs, Actors) ->
  receive
    {spawn_actor, Actor} ->
      Pid = join_actor:spawn_actor_here(Actor),
      location(Name, Super, Subs, [Pid | Actors]);

    {spawn_location, Data, Continuation} ->
      SubPid = spawn_here(Data),
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
          Super ! {unregister_location, self()},
          Ss = lists:map(fun wrap_up/1, Subs),
          As = lists:map(fun join_actor:wrap_up/1, Actors),
          join_reg:unregister_self(),
          Pid ! {Ref, {ok, {Name, Ss, As}}},
          join_forward:forward_on(Name)
      end;

    {print_status, Pid, Ref, Level} ->
      io:format(join_util:indentation(Level) ++ "|~n"),
      io:format(join_util:indentation(Level) ++ "|-- location ~p (Pid: ~p, Super: ~p):~n",
                [Name, self(), Super]),
      lists:foreach(fun(A) -> join_debug:print_status(A, Level+1) end, Actors),
      lists:foreach(fun(S) -> join_debug:print_status(S, Level+1) end, Subs),
      Pid ! {Ref, ok},
      location(Name, Super, Subs, Actors);

    Other ->
      ?WARNING("location ~p received invalid message: ~p~n",
               [self(), Other]),
      location(Name, Super, Subs, Actors)
  end.


% spawn a given actor at a given location
spawn_actor_at(Location, Actor, Channel, Args) ->
  join_reg:send(Location, {spawn_actor, {Actor, Channel, Args}}).


% migrate a location to another one.
% Location will become a sublocation of Destination.
% at the new location, `ok` will be sent on the Continuation channel.
% Continuation can also be `none`.
go(Location, Destination, Continuation) ->
  spawn(fun() ->
            % race condition?
            try join_reg:get_pid(Location) of
              SourcePid ->
                Data = wrap_up(SourcePid),
                join_reg:send(Destination, {spawn_location, Data, Continuation})
            catch
              throw:timeout -> ?WARNING("~p cannot find location ~p to go to~n",
                                         [Location, Destination])
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


% spawn given location data on the currently executing location process.
% does not register the location at its superlocation, but in the registry.
spawn_here(Data) ->
  Super = self(),
  spawn_at(Super, Data).

% Super is a pid.
% does not register the location at its superlocation, but in the registry.
spawn_at(Super, {Channel, Subs, Actors}) ->
  spawn(fun() ->
            % register in registry
            join_reg:register_self(Channel),
            % register actors
            As = lists:map(fun join_actor:spawn_actor_here/1, Actors),
            % register sub locations
            Ss = lists:map(fun spawn_here/1, Subs),
            location(Channel, Super, Ss, As)
        end).


% create a new location with given name (just for debugging) at a given
% superlocation.
create(Super, Name) ->
  Channel = join_util:create_id({loc, Name}),
  Data = {Channel, [], []},
  join_reg:send(Super, {spawn_location, Data, none}),
  Channel.

% creates the root location of a node.
% this currently has to be run once on each node.
create_root() ->
  Channel = root_of(node()),
  spawn_at(none, {Channel, [], []}),
  Channel.

% returns the location name of the root location of a given node.
root_of(Node) ->
  {root, Node}.
