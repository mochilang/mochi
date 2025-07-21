:- style_check(-singleton).
:- initialization(main).

main :-
    Nums = [1, 2],
    nth0(1, Nums, _, T1),
    nth0(1, Nums1, 3, T1),
    nth0(1, Nums1, R2), writeln(R2).
