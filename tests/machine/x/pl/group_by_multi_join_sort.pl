:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

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
    findall(_V37, (member(C, Customer), member(O, Orders), get_item(O, 'o_custkey', _V6), get_item(C, 'c_custkey', _V7), (_V6 == _V7), member(L, Lineitem), get_item(L, 'l_orderkey', _V8), get_item(O, 'o_orderkey', _V9), (_V8 == _V9), member(N, Nation), get_item(N, 'n_nationkey', _V10), get_item(C, 'c_nationkey', _V11), (_V10 == _V11), get_item(O, 'o_orderdate', _V12), get_item(O, 'o_orderdate', _V13), get_item(L, 'l_returnflag', _V14), (((((_V12 >= Start_date), _V13) < End_date), _V14) == "R"), get_item(G, 'key', _V15), get_item(_V15, 'c_custkey', _V16), get_item(G, 'key', _V17), get_item(_V17, 'c_name', _V18), findall(_V23, (member(X, G), true, get_item(X, 'l', _V19), get_item(_V19, 'l_extendedprice', _V20), get_item(X, 'l', _V21), get_item(_V21, 'l_discount', _V22), _V23 = (_V20 * (1 - _V22))), _V24), sum_list(_V24, _V25), get_item(G, 'key', _V26), get_item(_V26, 'c_acctbal', _V27), get_item(G, 'key', _V28), get_item(_V28, 'n_name', _V29), get_item(G, 'key', _V30), get_item(_V30, 'c_address', _V31), get_item(G, 'key', _V32), get_item(_V32, 'c_phone', _V33), get_item(G, 'key', _V34), get_item(_V34, 'c_comment', _V35), dict_create(_V36, map, [c_custkey-_V16, c_name-_V18, revenue-_V25, c_acctbal-_V27, n_name-_V29, c_address-_V31, c_phone-_V33, c_comment-_V35]), _V37 = _V36), _V38),
    Result = _V38,
    write(Result),
    nl,
    true.
