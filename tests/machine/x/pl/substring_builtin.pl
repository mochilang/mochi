:- style_check(-singleton).
:- initialization(main, main).
main :-
    Substring("mochi", 1, 4, _V0),
    write(_V0),
    nl,
    true.
