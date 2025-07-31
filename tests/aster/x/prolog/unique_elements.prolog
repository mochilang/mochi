:- initialization(main).
:- style_check(-singleton).
main :-
        list_to_set([1, 2, 2, 3, 3, 3], S),
        writeln(S).
