:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Id-1, Name-"A"]),
    dict_create(_V1, map, [Id-2, Name-"B"]),
    Nations = [_V0, _V1],
    dict_create(_V2, map, [Id-1, Nation-1]),
    dict_create(_V3, map, [Id-2, Nation-2]),
    Suppliers = [_V2, _V3],
    dict_create(_V4, map, [Part-100, Supplier-1, Cost-10, Qty-2]),
    dict_create(_V5, map, [Part-100, Supplier-2, Cost-20, Qty-1]),
    dict_create(_V6, map, [Part-200, Supplier-1, Cost-5, Qty-3]),
    Partsupp = [_V4, _V5, _V6],
    get_item(S, 'id', _V7),
    get_item(Ps, 'supplier', _V8),
    get_item(N, 'id', _V9),
    get_item(S, 'nation', _V10),
    get_item(N, 'name', _V11),
    get_item(Ps, 'part', _V12),
    get_item(Ps, 'cost', _V13),
    get_item(Ps, 'qty', _V14),
    dict_create(_V15, map, [Part-_V12, Value-(_V13 * _V14)]),
    findall(_V15, (member(Ps, Partsupp), member(S, Suppliers), (_V7 == _V8), member(N, Nations), (_V9 == _V10), (_V11 == "A")), _V16),
    Filtered = _V16,
    get_item(G, 'key', _V17),
    get_item(R, 'value', _V18),
    findall(_V18, (member(R, G), true), _V19),
    sum_list(_V19, _V20),
    dict_create(_V21, map, [Part-_V17, Total-_V20]),
    findall(_V21, (member(X, Filtered), true), _V22),
    Grouped = _V22,
    writeln(Grouped),
    true.
