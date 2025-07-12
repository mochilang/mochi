:- style_check(-singleton).
:- initialization(main, main).
main :-
    term_string(123, _V0),
    writeln(_V0),
    true.
