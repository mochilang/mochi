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
    
:- initialization(main, main).
main :-
    sum_list([1, 2, 3], _V0),
    length([1, 2, 3], _V1),
    _V1 > 0,
    _V2 is _V0 / _V1,
    _V3 is _V2,
    writeln(_V3),
    true.
