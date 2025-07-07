:- style_check(-singleton).
:- use_module(library(http/json)).
json(V) :- json_write_dict(current_output, V), nl.


    main :-
    dict_create(_V0, map, [a-1, b-2]),
    M = _V0,
    json(M)
    .
:- initialization(main, main).
