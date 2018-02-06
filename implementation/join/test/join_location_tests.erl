-module(join_location_tests).
-include_lib("eunit/include/eunit.hrl").


start_gproc() ->
  application:set_env(gproc, gproc_dist, all),
  application:ensure_all_started(gproc).

create_location_test_() ->
  {"A location can be created and is available in the registry",
   {setup,
    fun start_gproc/0,
    fun create_location/0}}.

create_location() ->
  Root = join_location:create_root(),
  _RootPid = join_reg:get_pid(Root),

  Child = join_location:create(Root, child),
  _ChildPid = join_reg:get_pid(Child).

