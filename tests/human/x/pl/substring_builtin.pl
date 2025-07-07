:- style_check(-singleton).
    main :-
    call(Substring, "mochi", 1, 4, _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
