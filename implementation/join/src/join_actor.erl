-module(join_actor).
-include("debug.hrl").
-export([]).

% a process needs to do some things when it is spawned:
%   - register itself in gproc
%   - register itself in the location

% spawn and register a behavior on a given channel, in a given location
% based on behavior syntax in ActorPi
spawn_at(Location, Bhv, Channel, Args) ->
  ?DEBUG("starting ~p at ~p", [Channel, Location]),
  join_location:register_self(Location),
  spawn(fun() ->
            gproc:reg({n, l, Channel}),
            apply(Bhv, [Channel | Args])
        end).
