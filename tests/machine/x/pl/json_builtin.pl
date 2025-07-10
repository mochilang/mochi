:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2]),
    M = _V0,
    Json(M, _V1),
    _V1,
    true.
