:- style_check(-singleton).
min(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), min_list(Items, R).
min(V, R) :-
    is_list(V), !, min_list(V, R).
min(_, _) :- throw(error('min expects list or group')).


    main :-
    Nums = [3, 1, 4],
    min(Nums, _V0),
    write(_V0),
    nl,
    call(Max, Nums, _V1),
    write(_V1),
    nl
    .
:- initialization(main, main).
