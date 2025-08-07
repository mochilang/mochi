
:- dynamic now_seed/1.
:- dynamic now_seeded/1.

init_now :-
    ( getenv('MOCHI_NOW_SEED', S), S \= '' ->
        atom_number(S, V),
        assertz(now_seed(V)),
        assertz(now_seeded(true))
    ;
        assertz(now_seed(0)),
        assertz(now_seeded(false))
    ).

mochi_now(T) :-
    ( now_seeded(true) ->
        retract(now_seed(S)),
        NS is (S*1664525 + 1013904223) mod 2147483647,
        assertz(now_seed(NS)),
        T = NS
    ;
        get_time(Time), T is floor(Time*1000000000)
    ).

:- initialization(init_now).

print_fmt(Fmt, Args, _) :- format(Fmt, Args).


:- arithmetic_function(len/1).
len(X, R) :-
    ( is_list(X) -> length(X, R)
    ; string(X) -> string_length(X, R)
    ).

:- initialization(main).
:- style_check(-singleton).

create_state_space_tree(Sequence, Current, Index, R) :-
    (Index =:= len(Sequence) ->
    writeln(Current),
    Return1 is 0 ;
    T2 is Index + 1,
    create_state_space_tree(Sequence, Current, T2, _),
    nth0(Index, Sequence, T1),
    append(Current, [T1], With_elem),
    T4 is Index + 1,
    create_state_space_tree(Sequence, With_elem, T4, _)),
    R = Return1.

generate_all_subsequences(Sequence, R) :-
    T2 is 0,
    create_state_space_tree(Sequence, [], T2, _),
    Return is 0,
    R = Return.

main :-
    mochi_now(Start),
    Seq = [1, 2, 3],
    generate_all_subsequences([1, 2, 3], _),
    Seq2 = ["A", "B", "C"],
    generate_all_subsequences(["A", "B", "C"], _),
    mochi_now(End),
    Dur0 is End - Start,
    Dur1 is Dur0 / 1000,
    floor(Dur1, Dur),
    print_fmt("{\n  \"duration_us\": ~d,\n  \"memory_bytes\": 0,\n  \"name\": \"main\"\n}", [Dur], _).
