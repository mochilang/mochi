#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [T0 | Rest] ->
            T = list_to_integer(T0),
            io:put_chars(run(T, Rest, []))
    end.

run(0, _, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [R0, C0 | Rest], Out) ->
    Rows = list_to_integer(R0),
    Cols = list_to_integer(C0),
    {Best, Tail} = rows(Rows, Cols, Rest, lists:duplicate(Cols, 0), 0),
    run(T - 1, Tail, [integer_to_list(Best) | Out]).

rows(0, _Cols, Rest, _H, Best) -> {Best, Rest};
rows(Rows, Cols, [S | Rest], H, Best) ->
    H2 = update(string:trim(S), H, 1, Cols, []),
    rows(Rows - 1, Cols, Rest, H2, erlang:max(Best, hist(H2)), H2).

rows(Rows, Cols, Rest, _H, Best, H2) -> rows(Rows, Cols, Rest, H2, Best).

update(_S, _H, I, Cols, Acc) when I > Cols -> lists:reverse(Acc);
update(S, [V | T], I, Cols, Acc) ->
    Ch = lists:nth(I, S),
    NV = if Ch =:= $1 -> V + 1; true -> 0 end,
    update(S, T, I + 1, Cols, [NV | Acc]).

hist(H) -> outer(H, H, 0).

outer([], _All, Best) -> Best;
outer([X | T], All, Best) ->
    outer(T, All, erlang:max(Best, inner([X | T], X, 1, Best))).

inner([], _Mn, _Len, Best) -> Best;
inner([X | T], Mn, Len, Best) ->
    Mn2 = erlang:min(Mn, X),
    Area = Mn2 * Len,
    inner(T, Mn2, Len + 1, erlang:max(Best, Area)).
