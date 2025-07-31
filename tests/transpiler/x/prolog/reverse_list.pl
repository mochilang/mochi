:- initialization(main).
:- style_check(-singleton).

main :-
    reverse([1,2,3], R),
    writeln(R).
