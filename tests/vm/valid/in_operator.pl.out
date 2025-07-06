:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


    main :-
    Xs = [1, 2, 3],
    contains(Xs, 2, _V0),
    write(_V0),
    nl,
    contains(Xs, 5, _V1),
    (\+ _V1 -> _V2 = true ; _V2 = false),
    write(_V2),
    nl
    .
:- initialization(main, main).
