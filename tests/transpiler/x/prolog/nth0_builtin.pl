:- initialization(main).
:- style_check(-singleton).

main :-
    nth0(1, [a,b,c], X),
    writeln(X).
