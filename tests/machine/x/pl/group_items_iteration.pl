:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [tag-"a", val-1]),
    dict_create(_V1, map, [tag-"a", val-2]),
    dict_create(_V2, map, [tag-"b", val-3]),
    Data = [_V0, _V1, _V2],
    findall(_V3, (member(D, Data), true, _V3 = G), _V4),
    Groups = _V4,
    Tmp = [],
    catch(
        (
            member(G, Groups),
                catch(
                    (
                        Total is 0,
                        catch(
                            (
                                get_item(G, 'items', _V5),
                                member(X, _V5),
                                    catch(
                                        (
                                            get_item(X, 'val', _V7),
                                            Total_6 is (Total_6 + _V7),
                                            true
                                        ), continue, true),
                                        fail
                                    ; true
                                ), break, true),
                                get_item(G, 'key', _V9),
                                dict_create(_V10, map, [tag-_V9, total-Total_6]),
                                append(Tmp_8, [_V10], _V11),
                                Tmp_8 = _V11,
                                true
                            ), continue, true),
                            fail
                        ; true
                    ), break, true),
                    findall(_V12, (member(R, Tmp_8), true, _V12 = R), _V13),
                    Result = _V13,
                    write(Result),
                    nl,
                    true.
