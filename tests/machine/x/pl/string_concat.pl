:- style_check(-singleton).
main :-
    write(("hello " + "world")),
    nl,
    true.
:- initialization(main, main).
