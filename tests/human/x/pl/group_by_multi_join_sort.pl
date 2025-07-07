:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


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
    to_list(Customer, _V17),
    to_list(Orders, _V20),
    to_list(Lineitem, _V23),
    to_list(Nation, _V26),
    findall(_V28, (member(C, _V17), member(O, _V20), get_dict(o_custkey, O, _V18), get_dict(c_custkey, C, _V19), _V18 = _V19, member(L, _V23), get_dict(l_orderkey, L, _V21), get_dict(o_orderkey, O, _V22), _V21 = _V22, member(N, _V26), get_dict(n_nationkey, N, _V24), get_dict(c_nationkey, C, _V25), _V24 = _V25, get_dict(o_orderdate, O, _V6), get_dict(o_orderdate, O, _V7), get_dict(l_returnflag, L, _V8), ((_V6 >= Start_date, _V7 < End_date), _V8 = "R"), get_dict(c_custkey, C, _V9), get_dict(c_name, C, _V10), get_dict(c_acctbal, C, _V11), get_dict(c_address, C, _V12), get_dict(c_phone, C, _V13), get_dict(c_comment, C, _V14), get_dict(n_name, N, _V15), dict_create(_V16, map, [c_custkey-_V9, c_name-_V10, c_acctbal-_V11, c_address-_V12, c_phone-_V13, c_comment-_V14, n_name-_V15]), _V27 = _V16, _V28 = _V27-C), _V29),
    group_pairs(_V29, [], _V30),
    findall(_V70-_V69, (member(G, _V30), get_dict(key, G, _V31), get_dict(c_custkey, _V31, _V32), get_dict(key, G, _V33), get_dict(c_name, _V33, _V34), get_dict('Items', G, _V35), to_list(_V35, _V42), findall(_V43, (member(X, _V42), get_dict(l, X, _V36), get_dict(l_extendedprice, _V36, _V37), get_dict(l, X, _V38), get_dict(l_discount, _V38, _V39), _V40 is 1 - _V39, _V41 is _V37 * _V40, _V43 = _V41), _V44), sum(_V44, _V45), get_dict(key, G, _V46), get_dict(c_acctbal, _V46, _V47), get_dict(key, G, _V48), get_dict(n_name, _V48, _V49), get_dict(key, G, _V50), get_dict(c_address, _V50, _V51), get_dict(key, G, _V52), get_dict(c_phone, _V52, _V53), get_dict(key, G, _V54), get_dict(c_comment, _V54, _V55), dict_create(_V56, map, [c_custkey-_V32, c_name-_V34, revenue-_V45, c_acctbal-_V47, n_name-_V49, c_address-_V51, c_phone-_V53, c_comment-_V55]), get_dict('Items', G, _V57), to_list(_V57, _V64), findall(_V65, (member(X, _V64), get_dict(l, X, _V58), get_dict(l_extendedprice, _V58, _V59), get_dict(l, X, _V60), get_dict(l_discount, _V60, _V61), _V62 is 1 - _V61, _V63 is _V59 * _V62, _V65 = _V63), _V66), sum(_V66, _V67), _V68 is -(_V67), _V70 = _V68, _V69 = _V56), _V71),
    keysort(_V71, _V72),
    findall(V, member(_-V, _V72), _V73),
    Result = _V73,
    write(Result),
    nl
    .
:- initialization(main, main).
