:- initialization(main, main).
main :-
    sum_list([1, 2, 3], _V0),
    _V1 is _V0,
    writeln(_V1),
    true.
