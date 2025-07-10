:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

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
    findall(_V24, (member(X, Filtered), true, get_item(G, 'key', _V18), findall(_V20, (member(R, G), true, get_item(R, 'value', _V19), _V20 = _V19), _V21), sum_list(_V21, _V22), dict_create(_V23, map, [part-_V18, total-_V22]), _V24 = _V23), _V25),
    Grouped = _V25,
    write(Grouped),
    nl,
    true.
