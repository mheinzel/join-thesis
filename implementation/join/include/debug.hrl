
%-define(DEBUG(F, A), ok).
-define(DEBUG(Format, Args), io:format("DEBUG:~p:~p: " ++ Format ++ "~n", [?FUNCTION_NAME, ?LINE | Args])).
-define(WARNING(Format, Args), io:format("WARNING:~p:~p: " ++ Format ++ "~n", [?FUNCTION_NAME, ?LINE | Args])).
