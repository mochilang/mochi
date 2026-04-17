#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [TStr | Rest] -> solve(Rest, list_to_integer(TStr))
    end.

solve(_, 0) -> ok;
solve([NStr | Rest], T) ->
    N = list_to_integer(NStr),
    {Strs, Tail} = lists:split(N, Rest),
    io:format("\"~s\"~n", [lcp(Strs)]),
    solve(Tail, T - 1).

lcp([First | Rest]) -> lcp_loop(First, [First | Rest]).
lcp_loop(Prefix, Strs) ->
    case lists:all(fun(S) -> lists:prefix(Prefix, S) end, Strs) of
        true -> Prefix;
        false -> lcp_loop(lists:sublist(Prefix, length(Prefix) - 1), Strs)
    end.
