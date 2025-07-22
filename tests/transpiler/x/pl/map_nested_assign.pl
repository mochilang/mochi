:- initialization(main).
:- style_check(-singleton).

main :-
    Data = map{outer: map{inner: 1}},
    Data1 = map{outer: map{inner: 2}},
    writeln(2).
