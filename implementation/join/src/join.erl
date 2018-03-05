-module(join).
-include("debug.hrl").
-export([
         send/2,
         go/3,
         def/2,
         def_location/3,
         def_globally/4
        ]).

% the primitives for building join definitions.
%
% NOTE:
% wherever a Location parameter occurs, the innermost enclosing location must
% be supplied!


% re-export
send(Channel, Payload) ->
  join_reg:send(Channel, Payload).

go(Location, Destination, Continuation) ->
  join_location:go(Location, Destination, Continuation).


% binary join pattern:
%   def X(U) | Y(V) > P
%   in Q
% PQ receives the channel names of X and Y
% and must return two functions {P, Q}.
% P receives the payloads U and V.
% Q has no parameters.
%
def(Location, PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = join_util:create_id(act),
  X = join_util:create_id(fwX),
  Y = join_util:create_id(fwY),
  Payloads = queue:new(),
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(Location, join_actor:definition(P), A, [X, Y, X, Payloads]),
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(Location, fun join_actor:forward/2, Y, [A]),
  spawn(Q),
  % return actor name just for debugging
  A.

% takes an additional location identifier (just for debugging).
% PQ now also receives a location name (as first parameter).
def_location(Location, NewLocName, PRQ) ->
  ?DEBUG("~p", [PRQ]),
  % new names
  A = join_util:create_id(act),
  X = join_util:create_id(fwX),
  Y = join_util:create_id(fwY),
  Payloads = queue:new(),
  % create location
  NewLocation = join_location:create(Location, NewLocName),
  % get user-supplied processes
  % def NewLocation [ X | Y > P  :  R] in Q
  {P, R, Q} = PRQ(NewLocation, X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(NewLocation, join_actor:definition(P),
                               A, [X, Y, X, Payloads]),  % P passed at runtime only in implementation
  join_location:spawn_actor_at(NewLocation,
                               fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(NewLocation,
                               fun join_actor:forward/2, Y, [A]),
  spawn(R),
  spawn(Q),
  % return actor name just for debugging
  A.

% define two channels X and Y globally.
% X and Y must be globally unique and can be used on connected nodes as well.
%
% global names should be registered at the nodes' root location.
def_globally(Location, X, Y, PQ) ->
  ?DEBUG("~p", [PQ]),
  % new names
  A = join_util:create_id(act),
  Payloads = queue:new(),
  % get user-supplied processes
  {P, Q} = PQ(X, Y),
  % spawn in parallel (no need to spawn lists)
  join_location:spawn_actor_at(Location, join_actor:definition(P),
                               A, [X, Y, X, Payloads]),  % P passed at runtime only in implementation
  join_location:spawn_actor_at(Location,
                               fun join_actor:forward/2, X, [A]),
  join_location:spawn_actor_at(Location,
                               fun join_actor:forward/2, Y, [A]),
  spawn(Q),
  % return actor name just for debugging
  A.
