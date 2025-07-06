:- style_check(-singleton).
count(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
count(V, R) :-
    string(V), !, string_chars(V, C), length(C, R).
count(V, R) :-
    is_list(V), !, length(V, R).
count(_, _) :- throw(error('count expects list or group')).


    main :-
    count([1, 2, 3], _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
