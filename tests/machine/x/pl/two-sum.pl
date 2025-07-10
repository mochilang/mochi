:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

len_any(Value, Len) :-
    string(Value), !, string_length(Value, Len).
len_any(Value, Len) :-
    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).
len_any(Value, Len) :- length(Value, Len).

twoSum(Nums, Target, _Res) :-
    len_any(Nums, _V0),
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
                    twoSum([2, 7, 11, 15], 9, _V0),
                    Result = _V0,
                    get_item(Result, 0, _V1),
                    write(_V1),
                    nl,
                    get_item(Result, 1, _V2),
                    write(_V2),
                    nl,
                    true.
