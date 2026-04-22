#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, []))
    end.

run(0, _Rest, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [R0, C0 | Rest], Out) ->
    Rows = list_to_integer(R0),
    Cols = list_to_integer(C0),
    {Dungeon, Tail} = read_rows(Rows, Cols, Rest, []),
    run(T - 1, Tail, [integer_to_list(solve(Dungeon, Cols)) | Out]).

read_rows(0, _Cols, Rest, Acc) -> {lists:reverse(Acc), Rest};
read_rows(Rows, Cols, Rest, Acc) ->
    {Row, Tail} = read_row(Cols, Rest, []),
    read_rows(Rows - 1, Cols, Tail, [Row | Acc]).

read_row(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
read_row(N, [X | Rest], Acc) -> read_row(N - 1, Rest, [list_to_integer(X) | Acc]).

solve(Dungeon, Cols) ->
    Inf = 1000000000,
    Dp0 = lists:duplicate(Cols + 1, Inf),
    Dp1 = lists:sublist(Dp0, Cols - 1) ++ [1, Inf],
    hd(lists:foldl(fun(Row, Dp) -> update(Row, Dp, Cols, []) end, Dp1, lists:reverse(Dungeon))).

update(_Row, Dp, Cols, _Acc) when Cols =< 0 -> Dp;
update(Row, Dp, Cols, Acc) ->
    J = Cols,
    Cell = lists:nth(J, Row),
    Best = erlang:min(lists:nth(J, Dp), lists:nth(J + 1, Dp)),
    Need = Best - Cell,
    Val = if Need =< 1 -> 1; true -> Need end,
    Prefix = lists:sublist(Dp, J - 1),
    Suffix = lists:nthtail(J, Dp),
    update(Row, Prefix ++ [Val] ++ Suffix, Cols - 1, Acc).
