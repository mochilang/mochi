:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).


        p__lambda0(D, Res) :-
        get_dict(tag, D, _V3),
        Res = _V3.

            main :-
    dict_create(_V0, map, [tag-"a", val-1]),
    dict_create(_V1, map, [tag-"a", val-2]),
    dict_create(_V2, map, [tag-"b", val-3]),
    Data = [_V0, _V1, _V2],
    to_list(Data, _V5),
    group_by(_V5, p__lambda0, _V6),
    findall(_V7, (member(G, _V6), get_dict('Items', G, _V4), _V7 = _V4), _V8),
    Groups = _V8,
    nb_setval(tmp, []),
    to_list(Groups, _V9),
    catch(
        (
            member(G, _V9),
            catch(
                (
                    nb_setval(total, 0),
                    get_dict(items, G, _V10),
                    to_list(_V10, _V11),
                    catch(
                        (
                            member(X, _V11),
                            catch(
                                (
                                    nb_getval(total, _V12),
                                    get_dict(val, X, _V13),
                                    _V14 is _V12 + _V13,
                                    nb_setval(total, _V14),
                                    true
                                ), continue, true),
                                fail
                                ;
                                true
                            )
                            , break, true),
                            true,
                        nb_getval(tmp, _V15),
                        get_dict(key, G, _V16),
                        nb_getval(total, _V17),
                        nb_getval(total, _V18),
                        dict_create(_V19, map, [tag-_V16, total-_V18]),
                        call(Append, _V15, _V19, _V20),
                        nb_setval(tmp, _V20),
                        true
                    ), continue, true),
                    fail
                    ;
                    true
                )
                , break, true),
                true,
            nb_getval(tmp, _V21),
            to_list(_V21, _V23),
            findall(_V25-_V24, (member(R, _V23), get_dict(tag, R, _V22), _V25 = _V22, _V24 = R), _V26),
            keysort(_V26, _V27),
            findall(V, member(_-V, _V27), _V28),
            Result = _V28,
            write(Result),
            nl
            .
:- initialization(main, main).
