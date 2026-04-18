#!/usr/bin/env escript

reverse_int(X) -> reverse_int(X, 0).
reverse_int(0, Ans) -> Ans;
reverse_int(X, Ans) ->
    Digit = X rem 10,
    X2 = X div 10,
    case (Ans > 214748364) orelse (Ans =:= 214748364 andalso Digit > 7) orelse
         (Ans < -214748364) orelse (Ans =:= -214748364 andalso Digit < -8) of
        true -> 0;
        false -> reverse_int(X2, Ans * 10 + Digit)
    end.

trim_cr(Line) -> lists:reverse(drop_cr(lists:reverse(Line))).
drop_cr([$\r | T]) -> T;
drop_cr(L) -> L.

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [trim_cr(L) || L <- string:split(binary_to_list(Data), "\n", all)],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            Outs = [integer_to_list(reverse_int(list_to_integer(string:trim(X)))) || X <- lists:sublist(Rest, T)],
            io:format("~s", [string:join(Outs, "\n")])
    end.
