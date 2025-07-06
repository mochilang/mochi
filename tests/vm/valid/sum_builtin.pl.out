:- style_check(-singleton).
sum(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
sum(V, R) :-
    is_list(V), !, sum_list(V, R).
sum(_, _) :- throw(error('sum expects list or group')).


    main :-
    sum([1, 2, 3], _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
