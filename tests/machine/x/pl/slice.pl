:- style_check(-singleton).
slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).

:- initialization(main, main).
main :-
    slice([1, 2, 3], 1, 3, _V0),
    writeln(_V0),
    slice([1, 2, 3], 0, 2, _V1),
    writeln(_V1),
    slice("hello", 1, 4, _V2),
    writeln(_V2),
    true.
