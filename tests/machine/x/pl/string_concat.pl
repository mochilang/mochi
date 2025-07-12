:- style_check(-singleton).
:- initialization(main, main).
main :-
    string_concat("hello ", "world", _V0),
    writeln(_V0),
    true.
