#!/usr/bin/env escript

convert(S, NumRows) when NumRows =< 1; NumRows >= length(S) -> S;
convert(S, NumRows) ->
    Cycle = 2 * NumRows - 2,
    lists:append([row_chars(S, NumRows, Row, Cycle) || Row <- lists:seq(0, NumRows - 1)]).

row_chars(S, NumRows, Row, Cycle) ->
    row_chars(S, NumRows, Row, Cycle, Row, []).
row_chars(S, NumRows, Row, Cycle, I, Acc) when I < length(S) ->
    Ch = lists:nth(I + 1, S),
    Diag = I + Cycle - 2 * Row,
    Acc1 = [Ch | Acc],
    Acc2 = case Row > 0 andalso Row < NumRows - 1 andalso Diag < length(S) of
        true -> [lists:nth(Diag + 1, S) | Acc1];
        false -> Acc1
    end,
    row_chars(S, NumRows, Row, Cycle, I + Cycle, Acc2);
row_chars(_, _, _, _, _, Acc) -> lists:reverse(Acc).

trim_cr(Line) -> lists:reverse(drop_cr(lists:reverse(Line))).
drop_cr([$\r | T]) -> T;
drop_cr(L) -> L.

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines0 = string:split(binary_to_list(Data), "\n", all),
    Lines = [trim_cr(L) || L <- Lines0],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            Outs = build(Rest, T, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n")])
    end.

build(_, 0, Acc) -> Acc;
build([S, Rows | Rest], T, Acc) ->
    build(Rest, T - 1, [convert(S, list_to_integer(string:trim(Rows))) | Acc]);
build(_, _, Acc) -> Acc.
