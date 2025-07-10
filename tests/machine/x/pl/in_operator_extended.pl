:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    Xs = [1, 2, 3],
    findall(_V0, (member(X, Xs), ((X mod 2) == 1), _V0 = X), _V1),
    Ys = _V1,
    contains(Ys, 1, _V2),
    write(_V2),
    nl,
    contains(Ys, 2, _V3),
    write(_V3),
    nl,
    dict_create(_V4, map, [a-1]),
    M = _V4,
    contains(M, "a", _V5),
    write(_V5),
    nl,
    contains(M, "b", _V6),
    write(_V6),
    nl,
    S = "hello",
    contains(S, "ell", _V7),
    write(_V7),
    nl,
    contains(S, "foo", _V8),
    write(_V8),
    nl,
    true.
