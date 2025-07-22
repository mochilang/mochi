:- initialization(main).
:- style_check(-singleton).

inc(C, R) :-
    get_dict(n, C, Tmp),
    get_dict(n, C, V1),
    V11 is Tmp + 1, put_dict(n, C, V11, C1),
    R = Return.

main :-
    C = map{tag: "Counter", n: 0},
    inc(map{tag: "Counter", n: 0}, _),
    writeln(0).
