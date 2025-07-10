:- style_check(-singleton).
:- initialization(main, main).
main :-
    Nums = [1, 2, 3],
    findall(_V1, (member(N, Nums), (N > 1), sum_list(N, _V0), _V1 = _V0), _V2),
    Result = _V2,
    write(Result),
    nl,
    true.
