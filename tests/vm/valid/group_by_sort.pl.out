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


        p__lambda0(I, Res) :-
        get_dict(cat, I, _V4),
        Res = _V4.

    main :-
    dict_create(_V0, map, [cat-"a", val-3]),
    dict_create(_V1, map, [cat-"a", val-1]),
    dict_create(_V2, map, [cat-"b", val-5]),
    dict_create(_V3, map, [cat-"b", val-2]),
    Items = [_V0, _V1, _V2, _V3],
    to_list(Items, _V20),
    group_by(_V20, p__lambda0, _V21),
    findall(_V23-_V22, (member(G, _V21), get_dict(key, G, _V5), get_dict('Items', G, _V6), to_list(_V6, _V8), findall(_V9, (member(X, _V8), get_dict(val, X, _V7), _V9 = _V7), _V10), sum(_V10, _V11), dict_create(_V12, map, [cat-_V5, total-_V11]), get_dict('Items', G, _V13), to_list(_V13, _V15), findall(_V16, (member(X, _V15), get_dict(val, X, _V14), _V16 = _V14), _V17), sum(_V17, _V18), _V19 is -(_V18), _V23 = _V19, _V22 = _V12), _V24),
    keysort(_V24, _V25),
    findall(V, member(_-V, _V25), _V26),
    Grouped = _V26,
    write(Grouped),
    nl
    .
:- initialization(main, main).
