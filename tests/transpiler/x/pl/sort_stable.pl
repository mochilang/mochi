:- initialization(main).
:- style_check(-singleton).

main :-
    Items = [map{n: 1, v: "a"}, map{n: 1, v: "b"}, map{n: 2, v: "c"}],
    Result = ["a", "b", "c"],
    writeln(["a", "b", "c"]).
