:- style_check(-singleton).
:- initialization(main).

main :-
    Xs = [1, 2, 3],
    Ys = [],
    (member(1, Ys) -> writeln(true) ; writeln(false)),
    (member(2, Ys) -> writeln(true) ; writeln(false)),
    M = {a: 1},
    (get_dict("a", M, _) -> writeln(true) ; writeln(false)),
    (get_dict("b", M, _) -> writeln(true) ; writeln(false)),
    S = "hello",
    (sub_string(S, _, _, _, "ell") -> writeln(true) ; writeln(false)),
    (sub_string(S, _, _, _, "foo") -> writeln(true) ; writeln(false)).
