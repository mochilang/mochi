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

:- initialization(main, main).
main :-
    nb_setval(matrix, [[1, 2], [3, 4]]),
    get_item(matrix, 1, _V0),
    set_item(_V0, 0, 5, _V1),
    set_item(matrix, 1, _V1, _V2),
    nb_setval(matrix, _V2),
    nb_getval(matrix, _V3),
    get_item(_V3, 1, _V4),
    get_item(_V4, 0, _V5),
    writeln(_V5),
    true.
