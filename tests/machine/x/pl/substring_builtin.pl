:- style_check(-singleton).
:- initialization(main, main).
main :-
    Substring("mochi", 1, 4, _V0),
    writeln(_V0),
    true.
