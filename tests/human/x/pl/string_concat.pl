:- style_check(-singleton).
    main :-
    string_concat("hello ", "world", _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
