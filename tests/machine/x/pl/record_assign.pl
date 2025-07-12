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

inc(C, _Res) :-
    get_item(C, 'n', _V0),
    set_item(C, 'n', (_V0 + 1), _V1),
    C_2 = _V1,

:- initialization(main, main).
main :-
    dict_create(_V0, p_counter, ['n'-0]),
    nb_setval(c, _V0),
    nb_getval(c, _V1),
    inc(_V1, _V2),
    _V2,
    get_item(c, 'n', _V3),
    writeln(_V3),
    true.
