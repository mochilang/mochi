:- style_check(-singleton).
    main :-
    term_string(123, _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
