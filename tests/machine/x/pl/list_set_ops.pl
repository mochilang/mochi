:- style_check(-singleton).
len_any(Value, Len) :-
    string(Value), !, string_length(Value, Len).
len_any(Value, Len) :-
    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).
len_any(Value, Len) :- length(Value, Len).

union(A, B, R) :- append(A, B, C), list_to_set(C, R).
except([], _, []).
except([H|T], B, R) :- memberchk(H, B), !, except(T, B, R).
except([H|T], B, [H|R]) :- except(T, B, R).

intersect(A, B, R) :- intersect(A, B, [], R).
intersect([], _, Acc, R) :- reverse(Acc, R).
intersect([H|T], B, Acc, R) :- memberchk(H, B), \+ memberchk(H, Acc), !, intersect(T, B, [H|Acc], R).
intersect([_|T], B, Acc, R) :- intersect(T, B, Acc, R).

:- initialization(main, main).
main :-
    union([1, 2], [2, 3], _V0),
    write(_V0),
    nl,
    except([1, 2, 3], [2], _V1),
    write(_V1),
    nl,
    intersect([1, 2, 3], [2, 4], _V2),
    write(_V2),
    nl,
    append([1, 2], [2, 3], _V3),
    len_any(_V3, _V4),
    _V5 is _V4,
    write(_V5),
    nl,
    true.
