#!/usr/bin/env escript

max_profit([]) -> 0;
max_profit([First | Rest]) -> max_profit(Rest, First, 0).

max_profit([], _MinPrice, Best) -> Best;
max_profit([P | Rest], MinPrice, Best) ->
    Best2 = erlang:max(Best, P - MinPrice),
    Min2 = erlang:min(MinPrice, P),
    max_profit(Rest, Min2, Best2).

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all)],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            {_, Outs} = build(T, Rest, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n")])
    end.

build(0, Rest, Acc) -> {Rest, Acc};
build(T, [NStr | Rest], Acc) ->
    N = list_to_integer(string:trim(NStr)),
    {ValsStr, Tail} = lists:split(N, Rest),
    Prices = [list_to_integer(string:trim(V)) || V <- ValsStr],
    build(T - 1, Tail, [integer_to_list(max_profit(Prices)) | Acc]).
