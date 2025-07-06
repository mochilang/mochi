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
    nb_setval(matrix, [[1, 2], [3, 4]]),
    nb_getval(matrix, _V0),
    get_item(_V0, 1, _V1),
    set_item(_V1, 0, 5, _V2),
    set_item(_V0, 1, _V2, _V3),
    nb_setval(matrix, _V3),
    nb_getval(matrix, _V4),
    get_item(_V4, 1, _V5),
    get_item(_V5, 0, _V6),
    write(_V6),
    nl
    .
:- initialization(main, main).
