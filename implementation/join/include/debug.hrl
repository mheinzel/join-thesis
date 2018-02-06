
%-define(DEBUG(F, A), ok).
-define(DEBUG(Format, Args), io:format("~p:~p: " ++ Format ++ "~n", [?FUNCTION_NAME, ?LINE | Args])).
