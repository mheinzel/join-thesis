-module(join_debug).
-export([
         print_status/1,
         print_status/2
        ]).

% synchronous
print_status(Loc) ->
  print_status(Loc, 0).

print_status(Loc, IndentLevel) when is_pid(Loc) ->
  Ref = make_ref(),
  Loc ! {print_status, self(), Ref, IndentLevel},
  receive
    {Ref, ok} -> ok
  after 500 ->
          erlang:error(timeout)
  end;
print_status(Loc, IndentLevel) ->
  try
    Pid = join_reg:get_pid(Loc, 100),
    print_status(Pid, IndentLevel)
  catch
    error:timeout ->
      io:format(join_util:indentation(IndentLevel) ++ "XXX timeout~n")
  end.


