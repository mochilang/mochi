:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    _V0 is 3,
    nb_setval(x, _V0),
    _V1 is 4,
    nb_setval(y, _V1),
    nb_getval(x, _V2),
    nb_getval(y, _V3),
    dict_create(_V4, map, [a-_V2, b-_V3]),
    nb_setval(m, _V4),
    nb_getval(m, _V5),
    get_item(_V5, "a", _V6),
    write(_V6),
    write(' '),
    nb_getval(m, _V7),
    get_item(_V7, "b", _V8),
    write(_V8),
    nl,
    true.
