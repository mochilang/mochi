:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    Xs = [1, 2, 3],
    contains(Xs, 2, _V0),
    writeln(_V0),
    contains(Xs, 5, _V1),
    (_V1 -> _V2 = false ; _V2 = true),
    writeln(_V2),
    true.
