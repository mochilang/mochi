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
    nb_setval(nums, [1, 2]),
    set_item(nums, 1, 3, _V0),
    nb_setval(nums, _V0),
    nb_getval(nums, _V1),
    get_item(_V1, 1, _V2),
    writeln(_V2),
    true.
