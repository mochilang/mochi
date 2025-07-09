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

slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).

contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).
TwoSum(Nums, Target, _Res) :-
    (string(Nums) -> string_length(Nums, _V0) ; length(Nums, _V0)),
    N is _V0,
    catch(
        (
            _V1 is N - 1,
            between(0, _V1, I),
                catch(
                    (
                        catch(
                            (
                                _V2 is N - 1,
                                between((I + 1), _V2, J),
                                    catch(
                                        (
                                            get_item(Nums, I, _V3),
                                            get_item(Nums, J, _V4),
                                            (((_V3 + _V4) == Target) ->
                                                _Res = [I, J].
                                            ; true
                                            ),
                                            true
                                        ), continue, true),
                                        fail
                                    ; true
                                ), break, true),
                                true
                            ), continue, true),
                            fail
                        ; true
                    ), break, true),
                    _Res = [(-1), (-1)].
                
                :- initialization(main, main).
                main :-
                    TwoSum([2, 7, 11, 15], 9, _V0),
                    Result = _V0,
                    get_item(Result, 0, _V1),
                    writeln(_V1),
                    get_item(Result, 1, _V2),
                    writeln(_V2),
                    true.
