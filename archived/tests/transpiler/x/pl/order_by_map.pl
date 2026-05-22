:- initialization(main).
:- style_check(-singleton).

main :-
    Data = [map{a: 1, b: 2}, map{a: 1, b: 1}, map{a: 0, b: 5}],
    Sorted = [map{a: 0, b: 5}, map{a: 1, b: 1}, map{a: 1, b: 2}],
    writeln([map{a: 0, b: 5}, map{a: 1, b: 1}, map{a: 1, b: 2}]).
