-module(join_forward).
-include("debug.hrl").
-export([
         forward_on/1
        ]).

% just receive anything and forward it to a given channel.
% the idea is to turn actors and locations into a forwarder when migrating, so
% any messages left in the mailbox or still ariving at the old pid reach their
% correct target.
%
% note that these are not safe to use when using gproc, since the old process
% needs to die immediately to allow the new one to register.
forward_on(Channel) ->
  % leave some time
  timer:sleep(1000), 

  % make sure someone new register on the channel
  try join_reg:get_pid(Channel, 200) of
    Pid when Pid=:=self() ->
      ?WARNING("(~p) still registered on ~p",
               [self(), Channel]),
      forward_on(Channel);
    _ ->
      % some-one else register, forward everything!
      forward_all(Channel)
  catch
    error:timeout ->
      ?WARNING("still no-one registered on ~p",
               [Channel]),
      forward_on(Channel)
  end.

% forward messages after someone else registered on the channel
forward_all(Channel) ->
  receive
    Message ->
      ?DEBUG("forwarding on ~p:~n  ~p~n",
             [Channel, Message]),
      join_reg:send(Channel, Message),
      forward_all(Channel)
  after 5000 ->
          ?DEBUG("retiring ~p...", [Channel]),
          ok
  end.



