:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


sum(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
sum(V, R) :-
    is_list(V), !, sum_list(V, R).
sum(_, _) :- throw(error('sum expects list or group')).


    main :-
    Nums = [1, 2, 3],
    to_list(Nums, _V1),
    findall(N, (member(N, _V1), N > 1), _V2),
    findall(_V3, (member(N, _V2), sum(N, _V0), _V3 = _V0), _V4),
    Result = _V4,
    write(Result),
    nl
    .
:- initialization(main, main).
