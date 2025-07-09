:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Cat-"a", Val-3]),
    dict_create(_V1, map, [Cat-"a", Val-1]),
    dict_create(_V2, map, [Cat-"b", Val-5]),
    dict_create(_V3, map, [Cat-"b", Val-2]),
    Items = [_V0, _V1, _V2, _V3],
    get_item(G, 'key', _V4),
    get_item(X, 'val', _V5),
    findall(_V5, (member(X, G), true), _V6),
    sum_list(_V6, _V7),
    dict_create(_V8, map, [Cat-_V4, Total-_V7]),
    findall(_V8, (member(I, Items), true), _V9),
    Grouped = _V9,
    writeln(Grouped),
    true.
