#!/usr/bin/env escript

max_profit(Prices) -> max_profit(Prices, -1000000000, 0, -1000000000, 0).

max_profit([], _Buy1, _Sell1, _Buy2, Sell2) -> Sell2;
max_profit([P | Rest], Buy1, Sell1, Buy2, Sell2) ->
    Buy1a = erlang:max(Buy1, -P),
    Sell1a = erlang:max(Sell1, Buy1a + P),
    Buy2a = erlang:max(Buy2, Sell1a - P),
    Sell2a = erlang:max(Sell2, Buy2a + P),
    max_profit(Rest, Buy1a, Sell1a, Buy2a, Sell2a).

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
