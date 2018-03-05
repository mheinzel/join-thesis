spawn_actor({Actor, Channel, Args}) ->
  spawn(fun() ->
            join:register_self(Channel),
            apply(Actor, [Channel | Args])
        end).
