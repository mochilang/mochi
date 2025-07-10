:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2, c-3]),
    M = _V0,
    dict_pairs(M, _, _V1),
    findall(V, member(_-V, _V1), _V2),
    write(_V2),
    nl,
    true.
