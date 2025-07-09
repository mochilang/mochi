:- style_check(-singleton).
get_item(Container, Key, Val) :-
        is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
    get_item(Container, Index, Val) :-
        string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
    get_item(List, Index, Val) :- nth0(Index, List, Val).
    
    set_item(Container, Key, Val, Out) :-
        is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).
    set_item(List, Index, Val, Out) :-
        nth0(Index, List, _, Rest),
        nth0(Index, Out, Val, Rest).
    
    contains(Container, Item, Res) :-
        is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
    contains(List, Item, Res) :-
        string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
    contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).
    
boom(_Res) :-
    writeln("boom"),
    _Res = true.

:- initialization(main, main).
main :-
    ((((1 < 2), (2 < 3)), (3 < 4)) -> _V0 = true ; _V0 = false),
    writeln(_V0),
    boom(_V1),
    ((((1 < 2), (2 > 3)), _V1) -> _V2 = true ; _V2 = false),
    writeln(_V2),
    boom(_V3),
    (((((1 < 2), (2 < 3)), (3 > 4)), _V3) -> _V4 = true ; _V4 = false),
    writeln(_V4),
    true.
