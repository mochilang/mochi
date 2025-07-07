:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


    main :-
    nb_setval(x, 3),
    nb_setval(y, 4),
    nb_getval(x, _V0),
    nb_getval(y, _V1),
    dict_create(_V2, map, ['a'-_V0, 'b'-_V1]),
    nb_setval(m, _V2),
    nb_getval(m, _V3),
    get_item(_V3, "a", _V4),
    write(_V4),
    write(' '),
    nb_getval(m, _V5),
    get_item(_V5, "b", _V6),
    write(_V6),
    nl
    .
:- initialization(main, main).
