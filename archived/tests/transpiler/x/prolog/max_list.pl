:- initialization(main).
:- style_check(-singleton).

main :-
    max_list([4,7,2], M),
    writeln(M).
