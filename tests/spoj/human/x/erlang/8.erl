#!/usr/bin/env escript
%% https://www.spoj.com/problems/CMPLS/
%% Extends a sequence assuming it comes from a minimal-degree polynomial.

main(_) ->
    T = read_int(),
    process_cases(T).

read_int() ->
    case io:fread("", "~d") of
        {ok, [N]} -> N
    end.

read_seq(0, Acc) -> lists:reverse(Acc);
read_seq(N, Acc) ->
    I = read_int(),
    read_seq(N - 1, [I | Acc]).

all_equal([]) -> true;
all_equal([_]) -> true;
all_equal([H|T]) -> lists:all(fun(X) -> X =:= H end, T).

diff([_]) -> [];
diff([A,B|T]) -> [B - A | diff([B|T])].

build_table(Row) -> build_table(Row, []).
build_table(Row, Acc) ->
    case length(Row) =:= 1 orelse all_equal(Row) of
        true -> lists:reverse([Row | Acc]);
        false -> build_table(diff(Row), [Row | Acc])
    end.

extend(Table, 0) -> Table;
extend(Table, C) -> extend(extend_once(Table), C - 1).

extend_once(Table) ->
    {Rows, _} = extend_rev(lists:reverse(Table), 0),
    lists:reverse(Rows).

extend_rev([], Carry) -> {[], Carry};
extend_rev([Row|Rest], Carry) ->
    Last = lists:last(Row),
    Val = Last + Carry,
    Row1 = Row ++ [Val],
    {RestRows, _} = extend_rev(Rest, Val),
    {[Row1 | RestRows], Val}.

process_cases(0) -> ok;
process_cases(T) ->
    S = read_int(),
    C = read_int(),
    Seq = read_seq(S, []),
    Table = build_table(Seq),
    ExtTable = extend(Table, C),
    Row0 = hd(ExtTable),
    Tail = lists:nthtail(S, Row0),
    Ext = lists:sublist(Tail, C),
    print_seq(Ext),
    if T > 1 -> io:format("~n", []); true -> ok end,
    process_cases(T - 1).

print_seq([]) -> ok;
print_seq([H]) -> io:format("~B", [H]);
print_seq([H|T]) -> io:format("~B ", [H]), print_seq(T).
