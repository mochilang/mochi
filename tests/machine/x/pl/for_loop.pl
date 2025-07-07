:- style_check(-singleton).
main :-
    _V0 is 4 - 1,
    (between(1, _V0, I),
        write(I),
        nl,
        fail
    ; true),
    true.
:- initialization(main, main).
