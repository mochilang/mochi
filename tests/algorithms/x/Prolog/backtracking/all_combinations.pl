
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

:- initialization(main).
:- style_check(-singleton).

create_all_state(Increment, Total, Level, Current, Result, R) :-
    (Level =:= 0 ->
    append(Result, [Current], Return1) ; true),
    I = Increment,
    append(Current, [I], Next_current),
    create_all_state(I + 1, Total, Level - 1, Next_current, Result, Result1),
    I1 is I + 1,
    append(Current, [I1], Next_current1),
    create_all_state(I1 + 1, Total, Level - 1, Next_current1, Result1, Result2),
    I2 is I1 + 1,
    append(Current, [I2], Next_current2),
    create_all_state(I2 + 1, Total, Level - 1, Next_current2, Result2, Result3),
    I3 is I2 + 1,
    append(Current, [I3], Next_current3),
    create_all_state(I3 + 1, Total, Level - 1, Next_current3, Result3, Result4),
    I4 is I3 + 1,
    append(Current, [I4], Next_current4),
    create_all_state(I4 + 1, Total, Level - 1, Next_current4, Result4, Result5),
    I5 is I4 + 1,
    Return2 = Result5,
    Return2 is 0,
    R = Return2.

generate_all_combinations(N, K, R) :-
    (K < 0 ; N < 0 ->
    Return1 = [] ; true),
    Result = [],
    create_all_state(1, N, K, [], [], Return2),
    Return2 is 0,
    R = Return2.

main :-
    mochi_now(Start),
    writeln(generate_all_combinations(4, 2)),
    writeln(generate_all_combinations(3, 1)),
    mochi_now(End),
    Dur0 is End - Start,
    Dur1 is Dur0 / 1000,
    floor(Dur1, Dur),
    print_fmt("{\n  \"duration_us\": ~d,\n  \"memory_bytes\": 0,\n  \"name\": \"main\"\n}", [Dur], _).
