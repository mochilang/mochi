:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


    main :-
    S = "catch",
    contains(S, "cat", _V0),
    write(_V0),
    nl,
    contains(S, "dog", _V1),
    write(_V1),
    nl
    .
:- initialization(main, main).
