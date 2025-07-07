:- style_check(-singleton).
avg(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
avg(V, R) :-
    is_list(V), !, avg_list(V, R).
avg(_, _) :- throw(error('avg expects list or group')).
avg_list([], 0).
avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.


    main :-
    avg([1, 2, 3], _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
