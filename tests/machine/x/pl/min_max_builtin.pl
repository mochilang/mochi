:- style_check(-singleton).
:- initialization(main, main).
main :-
    Nums = [3, 1, 4],
    min_list(Nums, _V0),
    _V1 is _V0,
    write(_V1),
    nl,
    max_list(Nums, _V2),
    _V3 is _V2,
    write(_V3),
    nl,
    true.
