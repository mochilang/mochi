:- initialization(main, main).
main :-
    string_length("mochi", _V0),
    _V1 is _V0,
    writeln(_V1),
    true.
