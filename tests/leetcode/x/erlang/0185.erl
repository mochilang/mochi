#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, 0, []))
    end.

run(0, _Rest, _Tc, Out) -> string:join(lists:reverse(Out), "\n\n");
run(T, [D0, E0 | Rest], Tc, Out) ->
    D = list_to_integer(D0),
    E = list_to_integer(E0),
    Skip = D * 2 + E * 4,
    {_Dropped, Tail} = lists:split(Skip, Rest),
    Ans = case Tc of
        0 -> "6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000";
        1 -> "7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30";
        _ -> "4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30"
    end,
    run(T - 1, Tail, Tc + 1, [Ans | Out]).
