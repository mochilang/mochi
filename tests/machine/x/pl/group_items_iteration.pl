:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).

count(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
count(V, R) :-
    string(V), !, string_chars(V, C), length(C, R).
count(V, R) :-
    is_list(V), !, length(V, R).
count(_, _) :- throw(error('count expects list or group')).

avg(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
avg(V, R) :-
    is_list(V), !, avg_list(V, R).
avg(_, _) :- throw(error('avg expects list or group')).
avg_list([], 0).
avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.

sum(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
sum(V, R) :-
    is_list(V), !, sum_list(V, R).
sum(_, _) :- throw(error('sum expects list or group')).

group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [tag-"a", val-1]),
    dict_create(_V1, map, [tag-"a", val-2]),
    dict_create(_V2, map, [tag-"b", val-3]),
    Data = [_V0, _V1, _V2],
    findall(_V6, (member(D, Data), true, get_item(D, 'tag', _V3), _V4 = _V3, dict_create(_V5, map, ['D'-D]), _V6 = _V4-_V5), _V7),
    group_pairs(_V7, [], _V8),
    findall(_V9, (member(G, _V8), _V9 = G), _V10),
    Groups = _V10,
    nb_setval(tmp, []),
    catch(
        (
            member(G, Groups),
                catch(
                    (
                        _V11 is 0,
                        nb_setval(total, _V11),
                        catch(
                            (
                                get_item(G, 'items', _V12),
                                member(X, _V12),
                                    catch(
                                        (
                                            nb_getval(total, _V13),
                                            get_item(X, 'val', _V14),
                                            _V15 is (_V13 + _V14),
                                            nb_setval(total, _V15),
                                            true
                                        ), continue, true),
                                        fail
                                    ; true
                                ), break, true),
                                nb_getval(tmp, _V16),
                                get_item(G, 'key', _V17),
                                nb_getval(total, _V18),
                                dict_create(_V19, map, [tag-_V17, total-_V18]),
                                append(_V16, [_V19], _V20),
                                nb_setval(tmp, _V20),
                                true
                            ), continue, true),
                            fail
                        ; true
                    ), break, true),
                    nb_getval(tmp, _V21),
                    findall(_V22, (member(R, _V21), true, _V22 = R), _V23),
                    Result = _V23,
                    write(Result),
                    nl,
                    true.
