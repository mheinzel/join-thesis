definition(P) ->
  fun Actor(A, X, Y, Flag, Payloads) ->
    receive
      {Channel, Payload} ->
        case Payloads of
          [] ->
            Actor(A, X, Y, Channel, [Payload | Payloads]);
          [H | T] ->
            case Flag of
              X ->
                case Channel of
                  X ->
                    Actor(A, X, Y, Flag, [Payload | Payloads]);
                  Y ->
                    spawn(fun() -> P(H, Payload) end),
                    Actor(A, X, Y, Flag, T)
                end;
              Y ->
                case Channel of
                  Y ->
                    Actor(A, X, Y, Flag, [Payload | Payloads]);
                  X ->
                    spawn(fun() -> P(Payload, H) end),
                    Actor(A, X, Y, Flag, T)
                end
            end
        end
    end
  end.
