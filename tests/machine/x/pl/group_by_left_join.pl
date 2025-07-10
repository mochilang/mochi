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
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    dict_create(_V2, map, [id-3, name-"Charlie"]),
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [id-100, customerid-1]),
    dict_create(_V4, map, [id-101, customerid-1]),
    dict_create(_V5, map, [id-102, customerid-2]),
    Orders = [_V3, _V4, _V5],
    findall(_V12, (member(C, Customers), findall(O, (member(O, Orders), get_item(O, 'customerid', _V6), get_item(C, 'id', _V7), (_V6 == _V7)), _V8), (_V8 = [] -> O = nil; member(O, _V8)), true, get_item(C, 'name', _V9), _V10 = _V9, dict_create(_V11, map, [C-C, O-O]), _V12 = _V10-_V11), _V13),
    group_pairs(_V13, [], _V14),
    findall(_V21, (member(G, _V14), get_item(G, 'key', _V15), findall(_V17, (member(R, G), get_item(R, 'o', _V16), _V16, _V17 = R), _V18), count(_V18, _V19), dict_create(_V20, map, [name-_V15, count-_V19]), _V21 = _V20), _V22),
    Stats = _V22,
    write("--- Group Left Join ---"),
    nl,
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'name', _V23),
                        write(_V23),
                        write(' '),
                        write("orders:"),
                        write(' '),
                        get_item(S, 'count', _V24),
                        write(_V24),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
