:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    dict_create(_V0, map, ['a'-1, 'b'-2]),
    M = _V0,
    contains(M, "a", _V1),
    writeln(_V1),
    contains(M, "c", _V2),
    writeln(_V2),
    true.
