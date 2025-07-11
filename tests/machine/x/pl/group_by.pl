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
    dict_create(_V0, map, [name-"Alice", age-30, city-"Paris"]),
    dict_create(_V1, map, [name-"Bob", age-15, city-"Hanoi"]),
    dict_create(_V2, map, [name-"Charlie", age-65, city-"Paris"]),
    dict_create(_V3, map, [name-"Diana", age-45, city-"Hanoi"]),
    dict_create(_V4, map, [name-"Eve", age-70, city-"Paris"]),
    dict_create(_V5, map, [name-"Frank", age-22, city-"Hanoi"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5],
    findall(_V9, (member(Person, People), true, get_item(Person, 'city', _V6), _V7 = _V6, dict_create(_V8, map, ['Person'-Person]), _V9 = _V7-_V8), _V10),
    group_pairs(_V10, [], _V11),
    findall(_V19, (member(G, _V11), get_item(G, 'key', _V12), count(G, _V13), findall(_V15, (member(P, G), true, get_item(P, 'age', _V14), _V15 = _V14), _V16), avg(_V16, _V17), dict_create(_V18, map, [city-_V12, count-_V13, avg_age-_V17]), _V19 = _V18), _V20),
    Stats = _V20,
    write("--- People grouped by city ---"),
    nl,
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'city', _V21),
                        write(_V21),
                        write(' '),
                        write(": count ="),
                        write(' '),
                        get_item(S, 'count', _V22),
                        write(_V22),
                        write(' '),
                        write(", avg_age ="),
                        write(' '),
                        get_item(S, 'avg_age', _V23),
                        write(_V23),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
