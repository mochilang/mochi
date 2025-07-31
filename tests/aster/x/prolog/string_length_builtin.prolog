:- initialization(main).
:- style_check(-singleton).
main :-
        string_length(hello, L),
        writeln(L).
