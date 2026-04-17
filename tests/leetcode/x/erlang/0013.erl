#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [TStr | Rest] ->
            T = list_to_integer(TStr),
            lists:foreach(fun(S) -> io:format("~B~n", [roman_to_int(S)]) end, lists:sublist(Rest, T))
    end.

value($I) -> 1;
value($V) -> 5;
value($X) -> 10;
value($L) -> 50;
value($C) -> 100;
value($D) -> 500;
value($M) -> 1000.

roman_to_int(S) -> roman_to_int(S, 0).
roman_to_int([], Acc) -> Acc;
roman_to_int([C], Acc) -> Acc + value(C);
roman_to_int([C, N | Rest], Acc) ->
    Cur = value(C),
    Next = value(N),
    roman_to_int([N | Rest], Acc + if Cur < Next -> -Cur; true -> Cur end).
