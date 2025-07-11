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
    dict_create(_V0, map, [alice-1]),
    nb_setval(scores, _V0),
    set_item(scores, "bob", 2, _V1),
    nb_setval(scores, _V1),
    nb_getval(scores, _V2),
    get_item(_V2, "bob", _V3),
    write(_V3),
    nl,
    true.
