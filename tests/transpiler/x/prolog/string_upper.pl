:- initialization(main).
:- style_check(-singleton).

main :-
    string_upper("hello", U),
    writeln(U).
