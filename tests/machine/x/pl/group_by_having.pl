:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [name-"Alice", city-"Paris"]),
    dict_create(_V1, map, [name-"Bob", city-"Hanoi"]),
    dict_create(_V2, map, [name-"Charlie", city-"Paris"]),
    dict_create(_V3, map, [name-"Diana", city-"Hanoi"]),
    dict_create(_V4, map, [name-"Eve", city-"Paris"]),
    dict_create(_V5, map, [name-"Frank", city-"Hanoi"]),
    dict_create(_V6, map, [name-"George", city-"Paris"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5, _V6],
    findall(_V10, (member(P, People), true, get_item(G, 'key', _V7), length(G, _V8), dict_create(_V9, map, [city-_V7, num-_V8]), _V10 = _V9), _V11),
    Big = _V11,
    Json(Big, _V12),
    _V12,
    true.
