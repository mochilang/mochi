:- style_check(-singleton).
:- initialization(main, main).
main :-
    string_concat("hello ", "world", _V0),
    write(_V0),
    nl,
    true.
