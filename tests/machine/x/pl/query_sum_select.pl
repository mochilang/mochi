:- style_check(-singleton).
:- initialization(main, main).
main :-
    Nums = [1, 2, 3],
    sum_list(N, _V0),
    findall(_V0, (member(N, Nums), (N > 1)), _V1),
    Result = _V1,
    writeln(Result),
    true.
