:- style_check(-singleton).
get_item(Container, Key, Val) :-
        is_dict(Container),
            !,
            string(Key) -> atom_string(A, Key); =(A, Key),
        get_dict(A, Container, Val)
get_item(Container, Index, Val) :-
        string(Container),
            !,
            string_chars(Container, Chars),
        nth0(Index, Chars, Val)
get_item(List, Index, Val) :-
    nth0(Index, List, Val)
twosum(Nums, Target, Res) :-
    catch(length(Nums, _V0), =(N, _V0), is(_V1, -(N, 1)), catch(between(0, _V1, I), catch(is(_V2, +(I, 1)), is(_V3, -(N, 1)), catch(between(_V2, _V3, J), catch(get_item(Nums, I, _V4), get_item(Nums, J, _V5), is(_V6, +(_V4, _V5)), =(_V6, Target) -> throw(return([I, J])); true, true, continue, true), fail; true, break, true), true, true, continue, true), fail; true, break, true), true, true, return(_V7), =(Res, _V7))
twosum(Nums, Target, Res) :-
        is(_V8, -(1)),
            is(_V9, -(1)),
        =(Res, [_V8, _V9])
main :-
        twosum([2, 7, 11, 15], 9, _V10),
            =(Result, _V10),
            get_item(Result, 0, _V11),
            write(_V11),
            nl,
            get_item(Result, 1, _V12),
            write(_V12),
        nl
:- initialization(main, main).
