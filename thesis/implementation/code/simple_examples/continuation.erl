def_single(PQ) ->
  join:def(fun(X, Token) ->
    % only give it one name
    {P, Q} = PQ(X),
    { fun(U, _) ->
        % after consuming the token, emit a new one
        join:send(Token, token), P(U)
      end,
      fun() ->
        % initially emit one token
        join:send(Token, token), Q()
      end
    } end).

% usage
def_single(fun(X) -> {
  fun(U) ->
    % P
  end,
  fun() ->
    % Q
  end
} end).
