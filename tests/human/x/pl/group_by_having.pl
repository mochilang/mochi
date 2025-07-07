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


:- use_module(library(http/json)).
json(V) :- json_write_dict(current_output, V), nl.


        p__lambda0(P, Res) :-
        get_dict(city, P, _V7),
        Res = _V7.

    main :-
    dict_create(_V0, map, [name-"Alice", city-"Paris"]),
    dict_create(_V1, map, [name-"Bob", city-"Hanoi"]),
    dict_create(_V2, map, [name-"Charlie", city-"Paris"]),
    dict_create(_V3, map, [name-"Diana", city-"Hanoi"]),
    dict_create(_V4, map, [name-"Eve", city-"Paris"]),
    dict_create(_V5, map, [name-"Frank", city-"Hanoi"]),
    dict_create(_V6, map, [name-"George", city-"Paris"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5, _V6],
    to_list(People, _V12),
    group_by(_V12, p__lambda0, _V13),
    findall(_V14, (member(G, _V13), get_dict(key, G, _V8), get_dict('Items', G, _V9), count(_V9, _V10), dict_create(_V11, map, [city-_V8, num-_V10]), _V14 = _V11), _V15),
    Big = _V15,
    json(Big)
    .
:- initialization(main, main).
