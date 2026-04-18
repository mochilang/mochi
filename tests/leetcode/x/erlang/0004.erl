#!/usr/bin/env escript

merge([], B) -> B;
merge(A, []) -> A;
merge([Ha | Ta], [Hb | Tb]) when Ha =< Hb -> [Ha | merge(Ta, [Hb | Tb])];
merge([Ha | Ta], [Hb | Tb]) -> [Hb | merge([Ha | Ta], Tb)].

median(A, B) ->
    M = merge(A, B),
    N = length(M),
    case N rem 2 of
        1 -> io_lib:format("~.1f", [lists:nth(N div 2 + 1, M) * 1.0]);
        _ -> io_lib:format("~.1f", [ (lists:nth(N div 2, M) + lists:nth(N div 2 + 1, M)) / 2.0 ])
    end.

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
    {AStr, Rest2} = lists:split(N, Rest),
    A = [list_to_integer(string:trim(V)) || V <- AStr],
    [MStr | Rest3] = Rest2,
    M = list_to_integer(string:trim(MStr)),
    {BStr, Rest4} = lists:split(M, Rest3),
    B = [list_to_integer(string:trim(V)) || V <- BStr],
    build(T - 1, Rest4, [lists:flatten(median(A, B)) | Acc]).
