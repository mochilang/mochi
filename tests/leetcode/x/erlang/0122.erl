#!/usr/bin/env escript

max_profit(Prices) -> max_profit(Prices, 0).

max_profit([A, B | Rest], Best) when B > A -> max_profit([B | Rest], Best + (B - A));
max_profit([_A, B | Rest], Best) -> max_profit([B | Rest], Best);
max_profit(_, Best) -> Best.

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
