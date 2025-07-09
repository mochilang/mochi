:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Tag-"a", Val-1]),
    dict_create(_V1, map, [Tag-"a", Val-2]),
    dict_create(_V2, map, [Tag-"b", Val-3]),
    Data = [_V0, _V1, _V2],
    findall(G, (member(D, Data), true), _V3),
    Groups = _V3,
    Tmp = [],
    catch(
        (
            member(G, Groups),
                catch(
                    (
                        Total is 0,
                        catch(
                            (
                                get_item(G, 'items', _V4),
                                member(X, _V4),
                                    catch(
                                        (
                                            get_item(X, 'val', _V6),
                                            Total_5 is (Total_5 + _V6),
                                            true
                                        ), continue, true),
                                        fail
                                    ; true
                                ), break, true),
                                get_item(G, 'key', _V8),
                                dict_create(_V9, map, [Tag-_V8, Total_5-Total_5]),
                                append(Tmp_7, [_V9], _V10),
                                Tmp_7 = _V10,
                                true
                            ), continue, true),
                            fail
                        ; true
                    ), break, true),
                    findall(R, (member(R, Tmp_7), true), _V11),
                    Result = _V11,
                    writeln(Result),
                    true.
