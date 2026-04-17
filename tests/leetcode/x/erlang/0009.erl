#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        _ ->
            [TStr | Rest] = Tokens,
            T = list_to_integer(TStr),
            Values = [list_to_integer(S) || S <- Rest],
            lists:foreach(
                fun(X) ->
                    Out = case is_palindrome(X) of
                        true -> "true";
                        false -> "false"
                    end,
                    io:format("~s~n", [Out])
                end,
                lists:sublist(Values, T)
            )
    end.

is_palindrome(X) when X < 0 -> false;
is_palindrome(X) -> X =:= reverse_digits(X, 0).

reverse_digits(0, Rev) -> Rev;
reverse_digits(X, Rev) -> reverse_digits(X div 10, Rev * 10 + (X rem 10)).
