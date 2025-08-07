
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

repeat_bool(Times, R) :-
    Res = [],
    I is 0,
    append([], [false], Res1),
    I1 is 1,
    append(Res1, [false], Res2),
    I2 is 2,
    append(Res2, [false], Res3),
    I3 is 3,
    append(Res3, [false], Res4),
    I4 is 4,
    append(Res4, [false], Res5),
    I5 is 5,
    Return1 = Res5,
    Return1 is 0,
    R = Return1.

set_bool(Xs, Idx, Value, R) :-
    Res = [],
    I is 0,
    (0 =:= Idx ->
    append([], [Value], Res1) ;
    append(Res1, [nth0(0, Xs, R)], Res2)),
    I1 is 1,
    (1 =:= Idx ->
    append(Res2, [Value], Res3) ;
    append(Res3, [nth0(1, Xs, R)], Res4)),
    I2 is 2,
    (2 =:= Idx ->
    append(Res4, [Value], Res5) ;
    append(Res5, [nth0(2, Xs, R)], Res6)),
    I3 is 3,
    (3 =:= Idx ->
    append(Res6, [Value], Res7) ;
    append(Res7, [nth0(3, Xs, R)], Res8)),
    I4 is 4,
    (4 =:= Idx ->
    append(Res8, [Value], Res9) ;
    append(Res9, [nth0(4, Xs, R)], Res10)),
    I5 is 5,
    Return1 = Res10,
    Return1 is 0,
    R = Return1.

create_state_space_tree(Sequence, Current, Index, Used, R) :-
    (Index =:= len(Sequence) ->
    writeln(Current),
    Return1 is 0 ; true),
    I is 0,
    (\+(nth0(0, Used, R)) ->
    append(Current, [nth0(0, Sequence, R)], Next_current),
    set_bool(Used, 0, true, Next_used),
    create_state_space_tree(Sequence, Next_current, Index + 1, Next_used, _) ; true),
    I1 is 1,
    (\+(nth0(1, Used, R)) ->
    append(Current, [nth0(1, Sequence, R)], Next_current1),
    set_bool(Used, 1, true, Next_used1),
    create_state_space_tree(Sequence, Next_current1, Index + 1, Next_used1, _) ; true),
    I2 is 2,
    (\+(nth0(2, Used, R)) ->
    append(Current, [nth0(2, Sequence, R)], Next_current2),
    set_bool(Used, 2, true, Next_used2),
    create_state_space_tree(Sequence, Next_current2, Index + 1, Next_used2, _) ; true),
    I3 is 3,
    (\+(nth0(3, Used, R)) ->
    append(Current, [nth0(3, Sequence, R)], Next_current3),
    set_bool(Used, 3, true, Next_used3),
    create_state_space_tree(Sequence, Next_current3, Index + 1, Next_used3, _) ; true),
    I4 is 4,
    (\+(nth0(4, Used, R)) ->
    append(Current, [nth0(4, Sequence, R)], Next_current4),
    set_bool(Used, 4, true, Next_used4),
    create_state_space_tree(Sequence, Next_current4, Index + 1, Next_used4, _) ; true),
    I5 is 5,
    R = Return1.

generate_all_permutations(Sequence, R) :-
    repeat_bool(len(Sequence), Used),
    create_state_space_tree(Sequence, [], 0, Used, _),
    Return is 0,
    R = Return.

main :-
    mochi_now(Start),
    Sequence = [3, 1, 2, 4],
    generate_all_permutations([3, 1, 2, 4], _),
    Sequence_2 = ["A", "B", "C"],
    generate_all_permutations(["A", "B", "C"], _),
    mochi_now(End),
    Dur0 is End - Start,
    Dur1 is Dur0 / 1000,
    floor(Dur1, Dur),
    print_fmt("{\n  \"duration_us\": ~d,\n  \"memory_bytes\": 0,\n  \"name\": \"main\"\n}", [Dur], _).
