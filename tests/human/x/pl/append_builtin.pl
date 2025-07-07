:- style_check(-singleton).
    main :-
    A = [1, 2],
    call(Append, A, 3, _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
