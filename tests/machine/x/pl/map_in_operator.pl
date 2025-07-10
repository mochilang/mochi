:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [1-"a", 2-"b"]),
    M = _V0,
    contains(M, 1, _V1),
    write(_V1),
    nl,
    contains(M, 3, _V2),
    write(_V2),
    nl,
    true.
