:- initialization(main).
:- style_check(-singleton).

main :-
    [H|_] = [a,b,c],
    writeln(H).
