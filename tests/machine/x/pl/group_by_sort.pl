:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [cat-"a", val-3]),
    dict_create(_V1, map, [cat-"a", val-1]),
    dict_create(_V2, map, [cat-"b", val-5]),
    dict_create(_V3, map, [cat-"b", val-2]),
    Items = [_V0, _V1, _V2, _V3],
    findall(_V10, (member(I, Items), true, get_item(G, 'key', _V4), findall(_V6, (member(X, G), true, get_item(X, 'val', _V5), _V6 = _V5), _V7), sum_list(_V7, _V8), dict_create(_V9, map, [cat-_V4, total-_V8]), _V10 = _V9), _V11),
    Grouped = _V11,
    write(Grouped),
    nl,
    true.
