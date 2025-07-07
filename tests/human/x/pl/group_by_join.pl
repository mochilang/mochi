:- style_check(-singleton).
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


group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).


        main :-
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    Customers = [_V0, _V1],
    dict_create(_V2, map, [id-100, customerid-1]),
    dict_create(_V3, map, [id-101, customerid-1]),
    dict_create(_V4, map, [id-102, customerid-2]),
    Orders = [_V2, _V3, _V4],
    to_list(Orders, _V6),
    to_list(Customers, _V9),
    findall(_V11, (member(O, _V6), member(C, _V9), get_dict(customerid, O, _V7), get_dict(id, C, _V8), _V7 = _V8, get_dict(name, C, _V5), _V10 = _V5, _V11 = _V10-O), _V12),
    group_pairs(_V12, [], _V13),
    findall(_V18, (member(G, _V13), get_dict(key, G, _V14), get_dict('Items', G, _V15), count(_V15, _V16), dict_create(_V17, map, [name-_V14, count-_V16]), _V18 = _V17), _V19),
    Stats = _V19,
    write("--- Orders per customer ---"),
    nl,
    to_list(Stats, _V20),
    catch(
        (
            member(S, _V20),
            catch(
                (
                    get_dict(name, S, _V21),
                    write(_V21),
                    write(' '),
                    write("orders:"),
                    write(' '),
                    get_dict(count, S, _V22),
                    write(_V22),
                    nl,
                    true
                ), continue, true),
                fail
                ;
                true
            )
            , break, true),
            true
        .
:- initialization(main, main).
