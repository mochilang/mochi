:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [n-1, v-"a"]),
    dict_create(_V1, map, [n-1, v-"b"]),
    dict_create(_V2, map, [n-2, v-"c"]),
    Items = [_V0, _V1, _V2],
    findall(_V4, (member(I, Items), true, get_item(I, 'v', _V3), _V4 = _V3), _V5),
    Result = _V5,
    write(Result),
    nl,
    true.
