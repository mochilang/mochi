:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2]),
    dict_create(_V1, map, [a-1, b-1]),
    dict_create(_V2, map, [a-0, b-5]),
    Data = [_V0, _V1, _V2],
    findall(_V3, (member(X, Data), true, _V3 = X), _V4),
    Sorted = _V4,
    write(Sorted),
    nl,
    true.
