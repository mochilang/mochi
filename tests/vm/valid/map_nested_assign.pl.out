:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).


    main :-
    dict_create(_V0, map, ['inner'-1]),
    dict_create(_V1, map, ['outer'-_V0]),
    nb_setval(data, _V1),
    nb_getval(data, _V2),
    get_item(_V2, "outer", _V3),
    set_item(_V3, "inner", 2, _V4),
    set_item(_V2, "outer", _V4, _V5),
    nb_setval(data, _V5),
    nb_getval(data, _V6),
    get_item(_V6, "outer", _V7),
    get_item(_V7, "inner", _V8),
    write(_V8),
    nl
    .
:- initialization(main, main).
