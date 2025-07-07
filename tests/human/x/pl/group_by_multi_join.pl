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


        p__lambda0(X, Res) :-
        get_dict(part, X, _V22),
        Res = _V22.

    main :-
    dict_create(_V0, map, [id-1, name-"A"]),
    dict_create(_V1, map, [id-2, name-"B"]),
    Nations = [_V0, _V1],
    dict_create(_V2, map, [id-1, nation-1]),
    dict_create(_V3, map, [id-2, nation-2]),
    Suppliers = [_V2, _V3],
    dict_create(_V4, map, [part-100, supplier-1, cost-10, qty-2]),
    dict_create(_V5, map, [part-100, supplier-2, cost-20, qty-1]),
    dict_create(_V6, map, [part-200, supplier-1, cost-5, qty-3]),
    Partsupp = [_V4, _V5, _V6],
    to_list(Partsupp, _V13),
    to_list(Suppliers, _V16),
    to_list(Nations, _V19),
    findall(_V20, (member(Ps, _V13), member(S, _V16), get_dict(id, S, _V14), get_dict(supplier, Ps, _V15), _V14 = _V15, member(N, _V19), get_dict(id, N, _V17), get_dict(nation, S, _V18), _V17 = _V18, get_dict(name, N, _V12), _V12 = "A", get_dict(part, Ps, _V7), get_dict(cost, Ps, _V8), get_dict(qty, Ps, _V9), _V10 is _V8 * _V9, dict_create(_V11, map, [part-_V7, value-_V10]), _V20 = _V11), _V21),
    Filtered = _V21,
    to_list(Filtered, _V31),
    group_by(_V31, p__lambda0, _V32),
    findall(_V33, (member(G, _V32), get_dict(key, G, _V23), get_dict('Items', G, _V24), to_list(_V24, _V26), findall(_V27, (member(R, _V26), get_dict(value, R, _V25), _V27 = _V25), _V28), sum(_V28, _V29), dict_create(_V30, map, [part-_V23, total-_V29]), _V33 = _V30), _V34),
    Grouped = _V34,
    write(Grouped),
    nl
    .
:- initialization(main, main).
