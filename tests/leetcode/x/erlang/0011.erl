#!/usr/bin/env escript

max_area(H) -> max_area(H, 0, length(H) - 1, 0).
max_area(_, L, R, Best) when L >= R -> Best;
max_area(H, L, R, Best) ->
    Left = lists:nth(L + 1, H),
    Right = lists:nth(R + 1, H),
    Height = erlang:min(Left, Right),
    Best2 = erlang:max(Best, (R - L) * Height),
    if Left < Right -> max_area(H, L + 1, R, Best2); true -> max_area(H, L, R - 1, Best2) end.

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
    H = [list_to_integer(string:trim(V)) || V <- ValsStr],
    build(T - 1, Tail, [integer_to_list(max_area(H)) | Acc]).
