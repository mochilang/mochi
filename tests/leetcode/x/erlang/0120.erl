#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, []))
    end.

run(0, _Rest, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [R0 | Rest], Out) ->
    Rows = list_to_integer(R0),
    {Tri, Tail} = read_tri(Rows, 1, Rest, []),
    run(T - 1, Tail, [integer_to_list(solve(Tri)) | Out]).

read_tri(Rows, R, Rest, Acc) when R > Rows -> {lists:reverse(Acc), Rest};
read_tri(Rows, R, Rest, Acc) ->
    {Row, Tail} = read_row(R, Rest, []),
    read_tri(Rows, R + 1, Tail, [Row | Acc]).

read_row(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
read_row(N, [X | Rest], Acc) -> read_row(N - 1, Rest, [list_to_integer(X) | Acc]).

solve(Tri) ->
    Dp0 = lists:last(Tri),
    Rows = lists:reverse(lists:sublist(Tri, length(Tri) - 1)),
    hd(lists:foldl(fun(Row, Dp) -> update(Row, Dp, 1, []) end, Dp0, Rows)).

update([], _Dp, _I, Acc) -> lists:reverse(Acc);
update([X | Row], Dp, I, Acc) ->
    A = lists:nth(I, Dp), B = lists:nth(I + 1, Dp),
    update(Row, Dp, I + 1, [X + erlang:min(A, B) | Acc]).
