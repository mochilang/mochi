:- initialization(main).
:- style_check(-singleton).
main :-
        sort([3, 1, 2], S),
        writeln(S).
