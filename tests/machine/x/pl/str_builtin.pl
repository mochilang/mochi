:- style_check(-singleton).
:- initialization(main, main).
main :-
    term_string(123, _V0),
    write(_V0),
    nl,
    true.
