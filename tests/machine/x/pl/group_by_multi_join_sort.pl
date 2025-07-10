:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).

count(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
count(V, R) :-
    string(V), !, string_chars(V, C), length(C, R).
count(V, R) :-
    is_list(V), !, length(V, R).
count(_, _) :- throw(error('count expects list or group')).

avg(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
avg(V, R) :-
    is_list(V), !, avg_list(V, R).
avg(_, _) :- throw(error('avg expects list or group')).
avg_list([], 0).
avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.

sum(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
sum(V, R) :-
    is_list(V), !, sum_list(V, R).
sum(_, _) :- throw(error('sum expects list or group')).

group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [n_nationkey-1, n_name-"BRAZIL"]),
    Nation = [_V0],
    dict_create(_V1, map, [c_custkey-1, c_name-"Alice", c_acctbal-100, c_nationkey-1, c_address-"123 St", c_phone-"123-456", c_comment-"Loyal"]),
    Customer = [_V1],
    dict_create(_V2, map, [o_orderkey-1000, o_custkey-1, o_orderdate-"1993-10-15"]),
    dict_create(_V3, map, [o_orderkey-2000, o_custkey-1, o_orderdate-"1994-01-02"]),
    Orders = [_V2, _V3],
    dict_create(_V4, map, [l_orderkey-1000, l_returnflag-"R", l_extendedprice-1000, l_discount-0.1]),
    dict_create(_V5, map, [l_orderkey-2000, l_returnflag-"N", l_extendedprice-500, l_discount-0]),
    Lineitem = [_V4, _V5],
    Start_date = "1993-10-01",
    End_date = "1994-01-01",
    findall(_V25, (member(C, Customer), member(O, Orders), get_item(O, 'o_custkey', _V6), get_item(C, 'c_custkey', _V7), (_V6 == _V7), member(L, Lineitem), get_item(L, 'l_orderkey', _V8), get_item(O, 'o_orderkey', _V9), (_V8 == _V9), member(N, Nation), get_item(N, 'n_nationkey', _V10), get_item(C, 'c_nationkey', _V11), (_V10 == _V11), get_item(O, 'o_orderdate', _V12), get_item(O, 'o_orderdate', _V13), get_item(L, 'l_returnflag', _V14), (((((_V12 >= Start_date), _V13) < End_date), _V14) == "R"), get_item(C, 'c_custkey', _V15), get_item(C, 'c_name', _V16), get_item(C, 'c_acctbal', _V17), get_item(C, 'c_address', _V18), get_item(C, 'c_phone', _V19), get_item(C, 'c_comment', _V20), get_item(N, 'n_name', _V21), dict_create(_V22, map, [c_custkey-_V15, c_name-_V16, c_acctbal-_V17, c_address-_V18, c_phone-_V19, c_comment-_V20, n_name-_V21]), _V23 = _V22, dict_create(_V24, map, [C-C, O-O, L-L, N-N]), _V25 = _V23-_V24), _V26),
    group_pairs(_V26, [], _V27),
    findall(_V50, (member(G, _V27), get_item(G, 'key', _V28), get_item(_V28, 'c_custkey', _V29), get_item(G, 'key', _V30), get_item(_V30, 'c_name', _V31), findall(_V36, (member(X, G), true, get_item(X, 'l', _V32), get_item(_V32, 'l_extendedprice', _V33), get_item(X, 'l', _V34), get_item(_V34, 'l_discount', _V35), _V36 = (_V33 * (1 - _V35))), _V37), sum(_V37, _V38), get_item(G, 'key', _V39), get_item(_V39, 'c_acctbal', _V40), get_item(G, 'key', _V41), get_item(_V41, 'n_name', _V42), get_item(G, 'key', _V43), get_item(_V43, 'c_address', _V44), get_item(G, 'key', _V45), get_item(_V45, 'c_phone', _V46), get_item(G, 'key', _V47), get_item(_V47, 'c_comment', _V48), dict_create(_V49, map, [c_custkey-_V29, c_name-_V31, revenue-_V38, c_acctbal-_V40, n_name-_V42, c_address-_V44, c_phone-_V46, c_comment-_V48]), _V50 = _V49), _V51),
    Result = _V51,
    write(Result),
    nl,
    true.
