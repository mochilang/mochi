:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    Xs = [1, 2, 3],
    findall(X, (member(X, Xs), ((X mod 2) == 1)), _V0),
    Ys = _V0,
    contains(Ys, 1, _V1),
    writeln(_V1),
    contains(Ys, 2, _V2),
    writeln(_V2),
    dict_create(_V3, map, [A-1]),
    M = _V3,
    contains(M, "a", _V4),
    writeln(_V4),
    contains(M, "b", _V5),
    writeln(_V5),
    S = "hello",
    contains(S, "ell", _V6),
    writeln(_V6),
    contains(S, "foo", _V7),
    writeln(_V7),
    true.
