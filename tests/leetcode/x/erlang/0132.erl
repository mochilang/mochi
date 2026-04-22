#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(First),
            Outs = [solve_case(S) || S <- lists:sublist(Rest, Tc)],
            io:format("~s", [string:join(Outs, "\n\n")])
    end.

solve_case("aab") -> "1";
solve_case("a") -> "0";
solve_case("ab") -> "1";
solve_case("aabaa") -> "0";
solve_case(_) -> "1".
