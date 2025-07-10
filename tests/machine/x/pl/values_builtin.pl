:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2, c-3]),
    M = _V0,
    Values(M, _V1),
    write(_V1),
    nl,
    true.
