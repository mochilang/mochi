:- style_check(-singleton).
contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


    main :-
    dict_create(_V0, map, ['a'-1, 'b'-2]),
    M = _V0,
    contains(M, "a", _V1),
    write(_V1),
    nl,
    contains(M, "c", _V2),
    write(_V2),
    nl
    .
:- initialization(main, main).
