% Generated by Mochi compiler v0.10.28 on 2025-07-18T08:21:12Z
:- style_check(-singleton).
set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).

print_val(V) :- number(V), !, format('~g', [V]).
print_val(V) :- write(V).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [alice-1]),
    nb_setval(scores, _V0),
    nb_getval(scores, _V1),
    set_item(_V1, "bob", 2, _V2),
    nb_setval(scores, _V2),
    nb_getval(scores, _V3),
    (string("bob") -> atom_string(_V5, "bob") ; _V5 = "bob"), get_dict(_V5, _V3, _V4),
    print_val(_V4), nl,
    true.
