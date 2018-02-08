join
=====

Prototype of a distributed Join Calculus DSL

Build
-----

```
$ rebar3 compile
```

Debug messages can be turned on or off in `include/debug.hrl`.

Trying it
---------

At the beginning of each `rebar3 shell` session run:

```erlang
Root = join_location:create_root().
```

Then you can try different examples.

```erlang
join_examples:print(Root).
join_examples:pi_channel(Root).
join_examples:beeper(Root).
```

Locations can be manually created and moved around.

```erlang
Child1 = join_location:create(Parent, child1).
Child2 = join_location:create(Parent, child2).
join:go(Child1, Child2, none).
```

You can check the status of a location tree.

```erlang
join_debug:print_status(Root).
```

To try it with multiple nodes, open multiple shells, giving the nodes names
and connecting them.

On one shell:

```
$ rebar3 shell --sname one
```

```erlang
> node().
'one@hostname'
> Root = join_location:create_root().
```

And on the other(s):

```
$ rebar3 shell --sname two
```

```erlang
> OtherNode = 'one@hostname'
> net_kernel:connect_node(OtherNode).
> Root = join_location:create_root().
> OtherRoot = join:location:root_of(OtherNode).
```

You can now move locations to the other root (and sublocations, if you
have their name). You could, for example, create the beeper on a local child
location and then move it to another node.

There is also a distributed example. After setting two nodes up, run

```erlang
join_examples:applet_server(Root).
```

just once on one node (it registers the global channel `cell`) and

```erlang
join_examples:applet_client(Root).
```

on the other.
