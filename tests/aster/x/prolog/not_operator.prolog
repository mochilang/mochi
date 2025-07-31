:- initialization(main).
:- style_check(-singleton).
main :-
    \+(member(5, [1, 2, 3])) -> writeln(true); writeln(false).
