:- style_check(-singleton).
:- initialization(main, main).
main :-
    _V0 is 4 - 1,
    (between(1, _V0, I),
        writeln(I),
        fail
    ; true),
    true.
