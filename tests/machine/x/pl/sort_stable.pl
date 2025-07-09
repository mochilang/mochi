:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [N-1, V-"a"]),
    dict_create(_V1, map, [N-1, V-"b"]),
    dict_create(_V2, map, [N-2, V-"c"]),
    Items = [_V0, _V1, _V2],
    get_item(I, 'v', _V3),
    findall(_V3, (member(I, Items), true), _V4),
    Result = _V4,
    writeln(Result),
    true.
