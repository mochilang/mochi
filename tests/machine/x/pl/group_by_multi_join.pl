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
    findall(_V16, (member(Ps, Partsupp), member(S, Suppliers), get_item(S, 'id', _V7), get_item(Ps, 'supplier', _V8), (_V7 == _V8), member(N, Nations), get_item(N, 'id', _V9), get_item(S, 'nation', _V10), (_V9 == _V10), get_item(N, 'name', _V11), (_V11 == "A"), get_item(Ps, 'part', _V12), get_item(Ps, 'cost', _V13), get_item(Ps, 'qty', _V14), dict_create(_V15, map, [part-_V12, value-(_V13 * _V14)]), _V16 = _V15), _V17),
    Filtered = _V17,
    findall(_V21, (member(X, Filtered), true, get_item(X, 'part', _V18), _V19 = _V18, dict_create(_V20, map, [X-X]), _V21 = _V19-_V20), _V22),
    group_pairs(_V22, [], _V23),
    findall(_V30, (member(G, _V23), get_item(G, 'key', _V24), findall(_V26, (member(R, G), true, get_item(R, 'value', _V25), _V26 = _V25), _V27), sum(_V27, _V28), dict_create(_V29, map, [part-_V24, total-_V28]), _V30 = _V29), _V31),
    Grouped = _V31,
    write(Grouped),
    nl,
    true.
