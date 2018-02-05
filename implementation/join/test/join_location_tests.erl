-module(join_location_tests).
-include_lib("eunit/include/eunit.hrl").


start_gproc() ->
  IsRunning = lists:member(gproc, application:which_applications()),
  if
    not(IsRunning) -> application:start(gproc)
  end.

create_location_test_() ->
  {"A location can be created and wrapped up",
   {setup,
    fun start_gproc/0,
    fun create_location/0}}.

create_location() ->
  Root = join_location:create_root(),
  % is also registered in gproc
  RootPid = join_actor:get_pid(Root),
  Child = join_location:create(Root, child),
  ChildPid = join_actor:get_pid(Child),
  ChildData = join_location:wrap_up(ChildPid),
  ?_assertEqual(ChildData, {Child, [], []}).

