:- style_check(-singleton).
:- initialization(main).

main :-
    Nums = [1, 2, 3],
    (member(2, Nums) -> writeln(true) ; writeln(false)),
    (member(4, Nums) -> writeln(true) ; writeln(false)).
