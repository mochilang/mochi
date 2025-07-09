:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

:- initialization(main, main).
main :-
    Nums = [1, 2, 3],
    contains(Nums, 2, _V0),
    writeln(_V0),
    contains(Nums, 4, _V1),
    writeln(_V1),
    true.
