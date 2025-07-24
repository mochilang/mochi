:- initialization(main).
:- style_check(-singleton).

binEval(Op, L, R, R) :-
    exprEval(L, Lv),
    exprEval(R, Rv),
    (Op =:= OP_ADD ->
    Return1 = map{tag: "Rational", num: (string_concat(get_dict(num, Lv, R) * get_dict(denom, Rv, R), get_dict(denom, Lv, R), T), T) * get_dict(num, Rv, R), denom: get_dict(denom, Lv, R) * get_dict(denom, Rv, R)} ; true),
    (Op =:= OP_SUB ->
    Return2 = map{tag: "Rational", num: get_dict(num, Lv, R) * get_dict(denom, Rv, R) - get_dict(denom, Lv, R) * get_dict(num, Rv, R), denom: get_dict(denom, Lv, R) * get_dict(denom, Rv, R)} ; true),
    (Op =:= OP_MUL ->
    Return3 = map{tag: "Rational", num: get_dict(num, Lv, R) * get_dict(num, Rv, R), denom: get_dict(denom, Lv, R) * get_dict(denom, Rv, R)} ; true),
    Return4 = map{tag: "Rational", num: get_dict(num, Lv, R) * get_dict(denom, Rv, R), denom: get_dict(denom, Lv, R) * get_dict(num, Rv, R)},
    R = Return4.

binString(Op, L, R, R) :-
    exprString(L, Ls),
    exprString(R, Rs),
    Opstr = "",
    (Op =:= OP_ADD ->
    Opstr1 = " + " ;
    (Op =:= OP_SUB ->
    Opstr2 = " - " ;
    (Op =:= OP_MUL ->
    Opstr3 = " * " ;
    Opstr4 = " / "))),
    Return1 = (string_concat((string_concat((string_concat("(", Ls, T), T), " / ", T), T) + Rs, ")", T), T),
    R = Return1.

newNum(N, R) :-
    Return1 = map{tag: "Num", value: map{tag: "Rational", num: N, denom: 1}},
    R = Return1.

exprEval(X, R) :-
    (X = map{tag: "Num", _1: V} -> Return1 = V ; (X = map{tag: "Bin", _1: Op, _2: L, _3: R} -> Return1 = binEval(Op, L, R) ; Return1 = 0)),
    R = Return1.

exprString(X, R) :-
    (X = map{tag: "Num", _1: V} -> Return1 = get_dict(num, V, R) ; (X = map{tag: "Bin", _1: Op, _2: L, _3: R} -> Return1 = binString(Op, L, R) ; Return1 = 0)),
    R = Return1.

solve(Xs, R) :-
    (len(Xs) =:= 1 ->
    exprEval(nth0(0, Xs, R), F),
    (get_dict(denom, F, R) \= 0 , get_dict(num, F, R) = get_dict(denom, F, R) * Goal ->
    exprString(nth0(0, Xs, R), R1), writeln(R1),
    Return1 = true ; true),
    Return2 = false ; true),
    I is 0,
    J is 1,
    Rest = [],
    K is 0,
    (false , 0 =\= 1 ->
    append([], [nth0(0, Xs, R)], Rest1) ; true),
    K1 is 1,
    (true , 1 =\= 1 ->
    append(Rest1, [nth0(1, Xs, R)], Rest2) ; true),
    K2 is 2,
    (true , 2 =\= 1 ->
    append(Rest2, [nth0(2, Xs, R)], Rest3) ; true),
    K3 is 3,
    (true , 3 =\= 1 ->
    append(Rest3, [nth0(3, Xs, R)], Rest4) ; true),
    K4 is 4,
    (true , 4 =\= 1 ->
    append(Rest4, [nth0(4, Xs, R)], Rest5) ; true),
    K5 is 5,
    A = nth0(0, Xs, R),
    B = nth0(1, Xs, R),
    Op = OP_ADD,
    Node = map{tag: "Bin", op: Op, left: A, right: B},
    (solve(append(Rest5, [map{tag: "Bin", op: Op, left: A, right: B}], R)) ->
    Return3 = true ; true),
    Op1 = OP_SUB,
    Node1 = map{tag: "Bin", op: Op1, left: A, right: B},
    (solve(append(Rest5, [map{tag: "Bin", op: Op1, left: A, right: B}], R)) ->
    Return4 = true ; true),
    Op2 = OP_MUL,
    Node2 = map{tag: "Bin", op: Op2, left: A, right: B},
    (solve(append(Rest5, [map{tag: "Bin", op: Op2, left: A, right: B}], R)) ->
    Return5 = true ; true),
    Op3 = OP_DIV,
    Node3 = map{tag: "Bin", op: Op3, left: A, right: B},
    (solve(append(Rest5, [map{tag: "Bin", op: Op3, left: A, right: B}], R)) ->
    Return6 = true ; true),
    Node4 = map{tag: "Bin", op: OP_SUB, left: B, right: A},
    (solve(append(Rest5, [map{tag: "Bin", op: OP_SUB, left: B, right: A}], R)) ->
    Return7 = true ; true),
    Node5 = map{tag: "Bin", op: OP_DIV, left: B, right: A},
    (solve(append(Rest5, [map{tag: "Bin", op: OP_DIV, left: B, right: A}], R)) ->
    Return8 = true ; true),
    J1 is 2,
    Rest6 = [],
    K6 is 0,
    (false , 0 =\= 2 ->
    append([], [nth0(0, Xs, R)], Rest7) ; true),
    K7 is 1,
    (true , 1 =\= 2 ->
    append(Rest7, [nth0(1, Xs, R)], Rest8) ; true),
    K8 is 2,
    (true , 2 =\= 2 ->
    append(Rest8, [nth0(2, Xs, R)], Rest9) ; true),
    K9 is 3,
    (true , 3 =\= 2 ->
    append(Rest9, [nth0(3, Xs, R)], Rest10) ; true),
    K10 is 4,
    (true , 4 =\= 2 ->
    append(Rest10, [nth0(4, Xs, R)], Rest11) ; true),
    K11 is 5,
    A1 = nth0(0, Xs, R),
    B1 = nth0(2, Xs, R),
    Op4 = OP_ADD,
    Node6 = map{tag: "Bin", op: Op4, left: A1, right: B1},
    (solve(append(Rest11, [map{tag: "Bin", op: Op4, left: A1, right: B1}], R)) ->
    Return9 = true ; true),
    Op5 = OP_SUB,
    Node7 = map{tag: "Bin", op: Op5, left: A1, right: B1},
    (solve(append(Rest11, [map{tag: "Bin", op: Op5, left: A1, right: B1}], R)) ->
    Return10 = true ; true),
    Op6 = OP_MUL,
    Node8 = map{tag: "Bin", op: Op6, left: A1, right: B1},
    (solve(append(Rest11, [map{tag: "Bin", op: Op6, left: A1, right: B1}], R)) ->
    Return11 = true ; true),
    Op7 = OP_DIV,
    Node9 = map{tag: "Bin", op: Op7, left: A1, right: B1},
    (solve(append(Rest11, [map{tag: "Bin", op: Op7, left: A1, right: B1}], R)) ->
    Return12 = true ; true),
    Node10 = map{tag: "Bin", op: OP_SUB, left: B1, right: A1},
    (solve(append(Rest11, [map{tag: "Bin", op: OP_SUB, left: B1, right: A1}], R)) ->
    Return13 = true ; true),
    Node11 = map{tag: "Bin", op: OP_DIV, left: B1, right: A1},
    (solve(append(Rest11, [map{tag: "Bin", op: OP_DIV, left: B1, right: A1}], R)) ->
    Return14 = true ; true),
    J2 is 3,
    Rest12 = [],
    K12 is 0,
    (false , 0 =\= 3 ->
    append([], [nth0(0, Xs, R)], Rest13) ; true),
    K13 is 1,
    (true , 1 =\= 3 ->
    append(Rest13, [nth0(1, Xs, R)], Rest14) ; true),
    K14 is 2,
    (true , 2 =\= 3 ->
    append(Rest14, [nth0(2, Xs, R)], Rest15) ; true),
    K15 is 3,
    (true , 3 =\= 3 ->
    append(Rest15, [nth0(3, Xs, R)], Rest16) ; true),
    K16 is 4,
    (true , 4 =\= 3 ->
    append(Rest16, [nth0(4, Xs, R)], Rest17) ; true),
    K17 is 5,
    A2 = nth0(0, Xs, R),
    B2 = nth0(3, Xs, R),
    Op8 = OP_ADD,
    Node12 = map{tag: "Bin", op: Op8, left: A2, right: B2},
    (solve(append(Rest17, [map{tag: "Bin", op: Op8, left: A2, right: B2}], R)) ->
    Return15 = true ; true),
    Op9 = OP_SUB,
    Node13 = map{tag: "Bin", op: Op9, left: A2, right: B2},
    (solve(append(Rest17, [map{tag: "Bin", op: Op9, left: A2, right: B2}], R)) ->
    Return16 = true ; true),
    Op10 = OP_MUL,
    Node14 = map{tag: "Bin", op: Op10, left: A2, right: B2},
    (solve(append(Rest17, [map{tag: "Bin", op: Op10, left: A2, right: B2}], R)) ->
    Return17 = true ; true),
    Op11 = OP_DIV,
    Node15 = map{tag: "Bin", op: Op11, left: A2, right: B2},
    (solve(append(Rest17, [map{tag: "Bin", op: Op11, left: A2, right: B2}], R)) ->
    Return18 = true ; true),
    Node16 = map{tag: "Bin", op: OP_SUB, left: B2, right: A2},
    (solve(append(Rest17, [map{tag: "Bin", op: OP_SUB, left: B2, right: A2}], R)) ->
    Return19 = true ; true),
    Node17 = map{tag: "Bin", op: OP_DIV, left: B2, right: A2},
    (solve(append(Rest17, [map{tag: "Bin", op: OP_DIV, left: B2, right: A2}], R)) ->
    Return20 = true ; true),
    J3 is 4,
    Rest18 = [],
    K18 is 0,
    (false , 0 =\= 4 ->
    append([], [nth0(0, Xs, R)], Rest19) ; true),
    K19 is 1,
    (true , 1 =\= 4 ->
    append(Rest19, [nth0(1, Xs, R)], Rest20) ; true),
    K20 is 2,
    (true , 2 =\= 4 ->
    append(Rest20, [nth0(2, Xs, R)], Rest21) ; true),
    K21 is 3,
    (true , 3 =\= 4 ->
    append(Rest21, [nth0(3, Xs, R)], Rest22) ; true),
    K22 is 4,
    (true , 4 =\= 4 ->
    append(Rest22, [nth0(4, Xs, R)], Rest23) ; true),
    K23 is 5,
    A3 = nth0(0, Xs, R),
    B3 = nth0(4, Xs, R),
    Op12 = OP_ADD,
    Node18 = map{tag: "Bin", op: Op12, left: A3, right: B3},
    (solve(append(Rest23, [map{tag: "Bin", op: Op12, left: A3, right: B3}], R)) ->
    Return21 = true ; true),
    Op13 = OP_SUB,
    Node19 = map{tag: "Bin", op: Op13, left: A3, right: B3},
    (solve(append(Rest23, [map{tag: "Bin", op: Op13, left: A3, right: B3}], R)) ->
    Return22 = true ; true),
    Op14 = OP_MUL,
    Node20 = map{tag: "Bin", op: Op14, left: A3, right: B3},
    (solve(append(Rest23, [map{tag: "Bin", op: Op14, left: A3, right: B3}], R)) ->
    Return23 = true ; true),
    Op15 = OP_DIV,
    Node21 = map{tag: "Bin", op: Op15, left: A3, right: B3},
    (solve(append(Rest23, [map{tag: "Bin", op: Op15, left: A3, right: B3}], R)) ->
    Return24 = true ; true),
    Node22 = map{tag: "Bin", op: OP_SUB, left: B3, right: A3},
    (solve(append(Rest23, [map{tag: "Bin", op: OP_SUB, left: B3, right: A3}], R)) ->
    Return25 = true ; true),
    Node23 = map{tag: "Bin", op: OP_DIV, left: B3, right: A3},
    (solve(append(Rest23, [map{tag: "Bin", op: OP_DIV, left: B3, right: A3}], R)) ->
    Return26 = true ; true),
    J4 is 5,
    Rest24 = [],
    K24 is 0,
    (false , 0 =\= 5 ->
    append([], [nth0(0, Xs, R)], Rest25) ; true),
    K25 is 1,
    (true , 1 =\= 5 ->
    append(Rest25, [nth0(1, Xs, R)], Rest26) ; true),
    K26 is 2,
    (true , 2 =\= 5 ->
    append(Rest26, [nth0(2, Xs, R)], Rest27) ; true),
    K27 is 3,
    (true , 3 =\= 5 ->
    append(Rest27, [nth0(3, Xs, R)], Rest28) ; true),
    K28 is 4,
    (true , 4 =\= 5 ->
    append(Rest28, [nth0(4, Xs, R)], Rest29) ; true),
    K29 is 5,
    A4 = nth0(0, Xs, R),
    B4 = nth0(5, Xs, R),
    Op16 = OP_ADD,
    Node24 = map{tag: "Bin", op: Op16, left: A4, right: B4},
    (solve(append(Rest29, [map{tag: "Bin", op: Op16, left: A4, right: B4}], R)) ->
    Return27 = true ; true),
    Op17 = OP_SUB,
    Node25 = map{tag: "Bin", op: Op17, left: A4, right: B4},
    (solve(append(Rest29, [map{tag: "Bin", op: Op17, left: A4, right: B4}], R)) ->
    Return28 = true ; true),
    Op18 = OP_MUL,
    Node26 = map{tag: "Bin", op: Op18, left: A4, right: B4},
    (solve(append(Rest29, [map{tag: "Bin", op: Op18, left: A4, right: B4}], R)) ->
    Return29 = true ; true),
    Op19 = OP_DIV,
    Node27 = map{tag: "Bin", op: Op19, left: A4, right: B4},
    (solve(append(Rest29, [map{tag: "Bin", op: Op19, left: A4, right: B4}], R)) ->
    Return30 = true ; true),
    Node28 = map{tag: "Bin", op: OP_SUB, left: B4, right: A4},
    (solve(append(Rest29, [map{tag: "Bin", op: OP_SUB, left: B4, right: A4}], R)) ->
    Return31 = true ; true),
    Node29 = map{tag: "Bin", op: OP_DIV, left: B4, right: A4},
    (solve(append(Rest29, [map{tag: "Bin", op: OP_DIV, left: B4, right: A4}], R)) ->
    Return32 = true ; true),
    J5 is 6,
    I1 is 1,
    J6 is 2,
    Rest30 = [],
    K30 is 0,
    (true , 0 =\= 2 ->
    append([], [nth0(0, Xs, R)], Rest31) ; true),
    K31 is 1,
    (false , 1 =\= 2 ->
    append(Rest31, [nth0(1, Xs, R)], Rest32) ; true),
    K32 is 2,
    (true , 2 =\= 2 ->
    append(Rest32, [nth0(2, Xs, R)], Rest33) ; true),
    K33 is 3,
    (true , 3 =\= 2 ->
    append(Rest33, [nth0(3, Xs, R)], Rest34) ; true),
    K34 is 4,
    (true , 4 =\= 2 ->
    append(Rest34, [nth0(4, Xs, R)], Rest35) ; true),
    K35 is 5,
    A5 = nth0(1, Xs, R),
    B5 = nth0(2, Xs, R),
    Op20 = OP_ADD,
    Node30 = map{tag: "Bin", op: Op20, left: A5, right: B5},
    (solve(append(Rest35, [map{tag: "Bin", op: Op20, left: A5, right: B5}], R)) ->
    Return33 = true ; true),
    Op21 = OP_SUB,
    Node31 = map{tag: "Bin", op: Op21, left: A5, right: B5},
    (solve(append(Rest35, [map{tag: "Bin", op: Op21, left: A5, right: B5}], R)) ->
    Return34 = true ; true),
    Op22 = OP_MUL,
    Node32 = map{tag: "Bin", op: Op22, left: A5, right: B5},
    (solve(append(Rest35, [map{tag: "Bin", op: Op22, left: A5, right: B5}], R)) ->
    Return35 = true ; true),
    Op23 = OP_DIV,
    Node33 = map{tag: "Bin", op: Op23, left: A5, right: B5},
    (solve(append(Rest35, [map{tag: "Bin", op: Op23, left: A5, right: B5}], R)) ->
    Return36 = true ; true),
    Node34 = map{tag: "Bin", op: OP_SUB, left: B5, right: A5},
    (solve(append(Rest35, [map{tag: "Bin", op: OP_SUB, left: B5, right: A5}], R)) ->
    Return37 = true ; true),
    Node35 = map{tag: "Bin", op: OP_DIV, left: B5, right: A5},
    (solve(append(Rest35, [map{tag: "Bin", op: OP_DIV, left: B5, right: A5}], R)) ->
    Return38 = true ; true),
    J7 is 3,
    Rest36 = [],
    K36 is 0,
    (true , 0 =\= 3 ->
    append([], [nth0(0, Xs, R)], Rest37) ; true),
    K37 is 1,
    (false , 1 =\= 3 ->
    append(Rest37, [nth0(1, Xs, R)], Rest38) ; true),
    K38 is 2,
    (true , 2 =\= 3 ->
    append(Rest38, [nth0(2, Xs, R)], Rest39) ; true),
    K39 is 3,
    (true , 3 =\= 3 ->
    append(Rest39, [nth0(3, Xs, R)], Rest40) ; true),
    K40 is 4,
    (true , 4 =\= 3 ->
    append(Rest40, [nth0(4, Xs, R)], Rest41) ; true),
    K41 is 5,
    A6 = nth0(1, Xs, R),
    B6 = nth0(3, Xs, R),
    Op24 = OP_ADD,
    Node36 = map{tag: "Bin", op: Op24, left: A6, right: B6},
    (solve(append(Rest41, [map{tag: "Bin", op: Op24, left: A6, right: B6}], R)) ->
    Return39 = true ; true),
    Op25 = OP_SUB,
    Node37 = map{tag: "Bin", op: Op25, left: A6, right: B6},
    (solve(append(Rest41, [map{tag: "Bin", op: Op25, left: A6, right: B6}], R)) ->
    Return40 = true ; true),
    Op26 = OP_MUL,
    Node38 = map{tag: "Bin", op: Op26, left: A6, right: B6},
    (solve(append(Rest41, [map{tag: "Bin", op: Op26, left: A6, right: B6}], R)) ->
    Return41 = true ; true),
    Op27 = OP_DIV,
    Node39 = map{tag: "Bin", op: Op27, left: A6, right: B6},
    (solve(append(Rest41, [map{tag: "Bin", op: Op27, left: A6, right: B6}], R)) ->
    Return42 = true ; true),
    Node40 = map{tag: "Bin", op: OP_SUB, left: B6, right: A6},
    (solve(append(Rest41, [map{tag: "Bin", op: OP_SUB, left: B6, right: A6}], R)) ->
    Return43 = true ; true),
    Node41 = map{tag: "Bin", op: OP_DIV, left: B6, right: A6},
    (solve(append(Rest41, [map{tag: "Bin", op: OP_DIV, left: B6, right: A6}], R)) ->
    Return44 = true ; true),
    J8 is 4,
    Rest42 = [],
    K42 is 0,
    (true , 0 =\= 4 ->
    append([], [nth0(0, Xs, R)], Rest43) ; true),
    K43 is 1,
    (false , 1 =\= 4 ->
    append(Rest43, [nth0(1, Xs, R)], Rest44) ; true),
    K44 is 2,
    (true , 2 =\= 4 ->
    append(Rest44, [nth0(2, Xs, R)], Rest45) ; true),
    K45 is 3,
    (true , 3 =\= 4 ->
    append(Rest45, [nth0(3, Xs, R)], Rest46) ; true),
    K46 is 4,
    (true , 4 =\= 4 ->
    append(Rest46, [nth0(4, Xs, R)], Rest47) ; true),
    K47 is 5,
    A7 = nth0(1, Xs, R),
    B7 = nth0(4, Xs, R),
    Op28 = OP_ADD,
    Node42 = map{tag: "Bin", op: Op28, left: A7, right: B7},
    (solve(append(Rest47, [map{tag: "Bin", op: Op28, left: A7, right: B7}], R)) ->
    Return45 = true ; true),
    Op29 = OP_SUB,
    Node43 = map{tag: "Bin", op: Op29, left: A7, right: B7},
    (solve(append(Rest47, [map{tag: "Bin", op: Op29, left: A7, right: B7}], R)) ->
    Return46 = true ; true),
    Op30 = OP_MUL,
    Node44 = map{tag: "Bin", op: Op30, left: A7, right: B7},
    (solve(append(Rest47, [map{tag: "Bin", op: Op30, left: A7, right: B7}], R)) ->
    Return47 = true ; true),
    Op31 = OP_DIV,
    Node45 = map{tag: "Bin", op: Op31, left: A7, right: B7},
    (solve(append(Rest47, [map{tag: "Bin", op: Op31, left: A7, right: B7}], R)) ->
    Return48 = true ; true),
    Node46 = map{tag: "Bin", op: OP_SUB, left: B7, right: A7},
    (solve(append(Rest47, [map{tag: "Bin", op: OP_SUB, left: B7, right: A7}], R)) ->
    Return49 = true ; true),
    Node47 = map{tag: "Bin", op: OP_DIV, left: B7, right: A7},
    (solve(append(Rest47, [map{tag: "Bin", op: OP_DIV, left: B7, right: A7}], R)) ->
    Return50 = true ; true),
    J9 is 5,
    Rest48 = [],
    K48 is 0,
    (true , 0 =\= 5 ->
    append([], [nth0(0, Xs, R)], Rest49) ; true),
    K49 is 1,
    (false , 1 =\= 5 ->
    append(Rest49, [nth0(1, Xs, R)], Rest50) ; true),
    K50 is 2,
    (true , 2 =\= 5 ->
    append(Rest50, [nth0(2, Xs, R)], Rest51) ; true),
    K51 is 3,
    (true , 3 =\= 5 ->
    append(Rest51, [nth0(3, Xs, R)], Rest52) ; true),
    K52 is 4,
    (true , 4 =\= 5 ->
    append(Rest52, [nth0(4, Xs, R)], Rest53) ; true),
    K53 is 5,
    A8 = nth0(1, Xs, R),
    B8 = nth0(5, Xs, R),
    Op32 = OP_ADD,
    Node48 = map{tag: "Bin", op: Op32, left: A8, right: B8},
    (solve(append(Rest53, [map{tag: "Bin", op: Op32, left: A8, right: B8}], R)) ->
    Return51 = true ; true),
    Op33 = OP_SUB,
    Node49 = map{tag: "Bin", op: Op33, left: A8, right: B8},
    (solve(append(Rest53, [map{tag: "Bin", op: Op33, left: A8, right: B8}], R)) ->
    Return52 = true ; true),
    Op34 = OP_MUL,
    Node50 = map{tag: "Bin", op: Op34, left: A8, right: B8},
    (solve(append(Rest53, [map{tag: "Bin", op: Op34, left: A8, right: B8}], R)) ->
    Return53 = true ; true),
    Op35 = OP_DIV,
    Node51 = map{tag: "Bin", op: Op35, left: A8, right: B8},
    (solve(append(Rest53, [map{tag: "Bin", op: Op35, left: A8, right: B8}], R)) ->
    Return54 = true ; true),
    Node52 = map{tag: "Bin", op: OP_SUB, left: B8, right: A8},
    (solve(append(Rest53, [map{tag: "Bin", op: OP_SUB, left: B8, right: A8}], R)) ->
    Return55 = true ; true),
    Node53 = map{tag: "Bin", op: OP_DIV, left: B8, right: A8},
    (solve(append(Rest53, [map{tag: "Bin", op: OP_DIV, left: B8, right: A8}], R)) ->
    Return56 = true ; true),
    J10 is 6,
    Rest54 = [],
    K54 is 0,
    (true , 0 =\= 6 ->
    append([], [nth0(0, Xs, R)], Rest55) ; true),
    K55 is 1,
    (false , 1 =\= 6 ->
    append(Rest55, [nth0(1, Xs, R)], Rest56) ; true),
    K56 is 2,
    (true , 2 =\= 6 ->
    append(Rest56, [nth0(2, Xs, R)], Rest57) ; true),
    K57 is 3,
    (true , 3 =\= 6 ->
    append(Rest57, [nth0(3, Xs, R)], Rest58) ; true),
    K58 is 4,
    (true , 4 =\= 6 ->
    append(Rest58, [nth0(4, Xs, R)], Rest59) ; true),
    K59 is 5,
    A9 = nth0(1, Xs, R),
    B9 = nth0(6, Xs, R),
    Op36 = OP_ADD,
    Node54 = map{tag: "Bin", op: Op36, left: A9, right: B9},
    (solve(append(Rest59, [map{tag: "Bin", op: Op36, left: A9, right: B9}], R)) ->
    Return57 = true ; true),
    Op37 = OP_SUB,
    Node55 = map{tag: "Bin", op: Op37, left: A9, right: B9},
    (solve(append(Rest59, [map{tag: "Bin", op: Op37, left: A9, right: B9}], R)) ->
    Return58 = true ; true),
    Op38 = OP_MUL,
    Node56 = map{tag: "Bin", op: Op38, left: A9, right: B9},
    (solve(append(Rest59, [map{tag: "Bin", op: Op38, left: A9, right: B9}], R)) ->
    Return59 = true ; true),
    Op39 = OP_DIV,
    Node57 = map{tag: "Bin", op: Op39, left: A9, right: B9},
    (solve(append(Rest59, [map{tag: "Bin", op: Op39, left: A9, right: B9}], R)) ->
    Return60 = true ; true),
    Node58 = map{tag: "Bin", op: OP_SUB, left: B9, right: A9},
    (solve(append(Rest59, [map{tag: "Bin", op: OP_SUB, left: B9, right: A9}], R)) ->
    Return61 = true ; true),
    Node59 = map{tag: "Bin", op: OP_DIV, left: B9, right: A9},
    (solve(append(Rest59, [map{tag: "Bin", op: OP_DIV, left: B9, right: A9}], R)) ->
    Return62 = true ; true),
    J11 is 7,
    I2 is 2,
    J12 is 3,
    Rest60 = [],
    K60 is 0,
    (true , 0 =\= 3 ->
    append([], [nth0(0, Xs, R)], Rest61) ; true),
    K61 is 1,
    (true , 1 =\= 3 ->
    append(Rest61, [nth0(1, Xs, R)], Rest62) ; true),
    K62 is 2,
    (false , 2 =\= 3 ->
    append(Rest62, [nth0(2, Xs, R)], Rest63) ; true),
    K63 is 3,
    (true , 3 =\= 3 ->
    append(Rest63, [nth0(3, Xs, R)], Rest64) ; true),
    K64 is 4,
    (true , 4 =\= 3 ->
    append(Rest64, [nth0(4, Xs, R)], Rest65) ; true),
    K65 is 5,
    A10 = nth0(2, Xs, R),
    B10 = nth0(3, Xs, R),
    Op40 = OP_ADD,
    Node60 = map{tag: "Bin", op: Op40, left: A10, right: B10},
    (solve(append(Rest65, [map{tag: "Bin", op: Op40, left: A10, right: B10}], R)) ->
    Return63 = true ; true),
    Op41 = OP_SUB,
    Node61 = map{tag: "Bin", op: Op41, left: A10, right: B10},
    (solve(append(Rest65, [map{tag: "Bin", op: Op41, left: A10, right: B10}], R)) ->
    Return64 = true ; true),
    Op42 = OP_MUL,
    Node62 = map{tag: "Bin", op: Op42, left: A10, right: B10},
    (solve(append(Rest65, [map{tag: "Bin", op: Op42, left: A10, right: B10}], R)) ->
    Return65 = true ; true),
    Op43 = OP_DIV,
    Node63 = map{tag: "Bin", op: Op43, left: A10, right: B10},
    (solve(append(Rest65, [map{tag: "Bin", op: Op43, left: A10, right: B10}], R)) ->
    Return66 = true ; true),
    Node64 = map{tag: "Bin", op: OP_SUB, left: B10, right: A10},
    (solve(append(Rest65, [map{tag: "Bin", op: OP_SUB, left: B10, right: A10}], R)) ->
    Return67 = true ; true),
    Node65 = map{tag: "Bin", op: OP_DIV, left: B10, right: A10},
    (solve(append(Rest65, [map{tag: "Bin", op: OP_DIV, left: B10, right: A10}], R)) ->
    Return68 = true ; true),
    J13 is 4,
    Rest66 = [],
    K66 is 0,
    (true , 0 =\= 4 ->
    append([], [nth0(0, Xs, R)], Rest67) ; true),
    K67 is 1,
    (true , 1 =\= 4 ->
    append(Rest67, [nth0(1, Xs, R)], Rest68) ; true),
    K68 is 2,
    (false , 2 =\= 4 ->
    append(Rest68, [nth0(2, Xs, R)], Rest69) ; true),
    K69 is 3,
    (true , 3 =\= 4 ->
    append(Rest69, [nth0(3, Xs, R)], Rest70) ; true),
    K70 is 4,
    (true , 4 =\= 4 ->
    append(Rest70, [nth0(4, Xs, R)], Rest71) ; true),
    K71 is 5,
    A11 = nth0(2, Xs, R),
    B11 = nth0(4, Xs, R),
    Op44 = OP_ADD,
    Node66 = map{tag: "Bin", op: Op44, left: A11, right: B11},
    (solve(append(Rest71, [map{tag: "Bin", op: Op44, left: A11, right: B11}], R)) ->
    Return69 = true ; true),
    Op45 = OP_SUB,
    Node67 = map{tag: "Bin", op: Op45, left: A11, right: B11},
    (solve(append(Rest71, [map{tag: "Bin", op: Op45, left: A11, right: B11}], R)) ->
    Return70 = true ; true),
    Op46 = OP_MUL,
    Node68 = map{tag: "Bin", op: Op46, left: A11, right: B11},
    (solve(append(Rest71, [map{tag: "Bin", op: Op46, left: A11, right: B11}], R)) ->
    Return71 = true ; true),
    Op47 = OP_DIV,
    Node69 = map{tag: "Bin", op: Op47, left: A11, right: B11},
    (solve(append(Rest71, [map{tag: "Bin", op: Op47, left: A11, right: B11}], R)) ->
    Return72 = true ; true),
    Node70 = map{tag: "Bin", op: OP_SUB, left: B11, right: A11},
    (solve(append(Rest71, [map{tag: "Bin", op: OP_SUB, left: B11, right: A11}], R)) ->
    Return73 = true ; true),
    Node71 = map{tag: "Bin", op: OP_DIV, left: B11, right: A11},
    (solve(append(Rest71, [map{tag: "Bin", op: OP_DIV, left: B11, right: A11}], R)) ->
    Return74 = true ; true),
    J14 is 5,
    Rest72 = [],
    K72 is 0,
    (true , 0 =\= 5 ->
    append([], [nth0(0, Xs, R)], Rest73) ; true),
    K73 is 1,
    (true , 1 =\= 5 ->
    append(Rest73, [nth0(1, Xs, R)], Rest74) ; true),
    K74 is 2,
    (false , 2 =\= 5 ->
    append(Rest74, [nth0(2, Xs, R)], Rest75) ; true),
    K75 is 3,
    (true , 3 =\= 5 ->
    append(Rest75, [nth0(3, Xs, R)], Rest76) ; true),
    K76 is 4,
    (true , 4 =\= 5 ->
    append(Rest76, [nth0(4, Xs, R)], Rest77) ; true),
    K77 is 5,
    A12 = nth0(2, Xs, R),
    B12 = nth0(5, Xs, R),
    Op48 = OP_ADD,
    Node72 = map{tag: "Bin", op: Op48, left: A12, right: B12},
    (solve(append(Rest77, [map{tag: "Bin", op: Op48, left: A12, right: B12}], R)) ->
    Return75 = true ; true),
    Op49 = OP_SUB,
    Node73 = map{tag: "Bin", op: Op49, left: A12, right: B12},
    (solve(append(Rest77, [map{tag: "Bin", op: Op49, left: A12, right: B12}], R)) ->
    Return76 = true ; true),
    Op50 = OP_MUL,
    Node74 = map{tag: "Bin", op: Op50, left: A12, right: B12},
    (solve(append(Rest77, [map{tag: "Bin", op: Op50, left: A12, right: B12}], R)) ->
    Return77 = true ; true),
    Op51 = OP_DIV,
    Node75 = map{tag: "Bin", op: Op51, left: A12, right: B12},
    (solve(append(Rest77, [map{tag: "Bin", op: Op51, left: A12, right: B12}], R)) ->
    Return78 = true ; true),
    Node76 = map{tag: "Bin", op: OP_SUB, left: B12, right: A12},
    (solve(append(Rest77, [map{tag: "Bin", op: OP_SUB, left: B12, right: A12}], R)) ->
    Return79 = true ; true),
    Node77 = map{tag: "Bin", op: OP_DIV, left: B12, right: A12},
    (solve(append(Rest77, [map{tag: "Bin", op: OP_DIV, left: B12, right: A12}], R)) ->
    Return80 = true ; true),
    J15 is 6,
    Rest78 = [],
    K78 is 0,
    (true , 0 =\= 6 ->
    append([], [nth0(0, Xs, R)], Rest79) ; true),
    K79 is 1,
    (true , 1 =\= 6 ->
    append(Rest79, [nth0(1, Xs, R)], Rest80) ; true),
    K80 is 2,
    (false , 2 =\= 6 ->
    append(Rest80, [nth0(2, Xs, R)], Rest81) ; true),
    K81 is 3,
    (true , 3 =\= 6 ->
    append(Rest81, [nth0(3, Xs, R)], Rest82) ; true),
    K82 is 4,
    (true , 4 =\= 6 ->
    append(Rest82, [nth0(4, Xs, R)], Rest83) ; true),
    K83 is 5,
    A13 = nth0(2, Xs, R),
    B13 = nth0(6, Xs, R),
    Op52 = OP_ADD,
    Node78 = map{tag: "Bin", op: Op52, left: A13, right: B13},
    (solve(append(Rest83, [map{tag: "Bin", op: Op52, left: A13, right: B13}], R)) ->
    Return81 = true ; true),
    Op53 = OP_SUB,
    Node79 = map{tag: "Bin", op: Op53, left: A13, right: B13},
    (solve(append(Rest83, [map{tag: "Bin", op: Op53, left: A13, right: B13}], R)) ->
    Return82 = true ; true),
    Op54 = OP_MUL,
    Node80 = map{tag: "Bin", op: Op54, left: A13, right: B13},
    (solve(append(Rest83, [map{tag: "Bin", op: Op54, left: A13, right: B13}], R)) ->
    Return83 = true ; true),
    Op55 = OP_DIV,
    Node81 = map{tag: "Bin", op: Op55, left: A13, right: B13},
    (solve(append(Rest83, [map{tag: "Bin", op: Op55, left: A13, right: B13}], R)) ->
    Return84 = true ; true),
    Node82 = map{tag: "Bin", op: OP_SUB, left: B13, right: A13},
    (solve(append(Rest83, [map{tag: "Bin", op: OP_SUB, left: B13, right: A13}], R)) ->
    Return85 = true ; true),
    Node83 = map{tag: "Bin", op: OP_DIV, left: B13, right: A13},
    (solve(append(Rest83, [map{tag: "Bin", op: OP_DIV, left: B13, right: A13}], R)) ->
    Return86 = true ; true),
    J16 is 7,
    Rest84 = [],
    K84 is 0,
    (true , 0 =\= 7 ->
    append([], [nth0(0, Xs, R)], Rest85) ; true),
    K85 is 1,
    (true , 1 =\= 7 ->
    append(Rest85, [nth0(1, Xs, R)], Rest86) ; true),
    K86 is 2,
    (false , 2 =\= 7 ->
    append(Rest86, [nth0(2, Xs, R)], Rest87) ; true),
    K87 is 3,
    (true , 3 =\= 7 ->
    append(Rest87, [nth0(3, Xs, R)], Rest88) ; true),
    K88 is 4,
    (true , 4 =\= 7 ->
    append(Rest88, [nth0(4, Xs, R)], Rest89) ; true),
    K89 is 5,
    A14 = nth0(2, Xs, R),
    B14 = nth0(7, Xs, R),
    Op56 = OP_ADD,
    Node84 = map{tag: "Bin", op: Op56, left: A14, right: B14},
    (solve(append(Rest89, [map{tag: "Bin", op: Op56, left: A14, right: B14}], R)) ->
    Return87 = true ; true),
    Op57 = OP_SUB,
    Node85 = map{tag: "Bin", op: Op57, left: A14, right: B14},
    (solve(append(Rest89, [map{tag: "Bin", op: Op57, left: A14, right: B14}], R)) ->
    Return88 = true ; true),
    Op58 = OP_MUL,
    Node86 = map{tag: "Bin", op: Op58, left: A14, right: B14},
    (solve(append(Rest89, [map{tag: "Bin", op: Op58, left: A14, right: B14}], R)) ->
    Return89 = true ; true),
    Op59 = OP_DIV,
    Node87 = map{tag: "Bin", op: Op59, left: A14, right: B14},
    (solve(append(Rest89, [map{tag: "Bin", op: Op59, left: A14, right: B14}], R)) ->
    Return90 = true ; true),
    Node88 = map{tag: "Bin", op: OP_SUB, left: B14, right: A14},
    (solve(append(Rest89, [map{tag: "Bin", op: OP_SUB, left: B14, right: A14}], R)) ->
    Return91 = true ; true),
    Node89 = map{tag: "Bin", op: OP_DIV, left: B14, right: A14},
    (solve(append(Rest89, [map{tag: "Bin", op: OP_DIV, left: B14, right: A14}], R)) ->
    Return92 = true ; true),
    J17 is 8,
    I3 is 3,
    J18 is 4,
    Rest90 = [],
    K90 is 0,
    (true , 0 =\= 4 ->
    append([], [nth0(0, Xs, R)], Rest91) ; true),
    K91 is 1,
    (true , 1 =\= 4 ->
    append(Rest91, [nth0(1, Xs, R)], Rest92) ; true),
    K92 is 2,
    (true , 2 =\= 4 ->
    append(Rest92, [nth0(2, Xs, R)], Rest93) ; true),
    K93 is 3,
    (false , 3 =\= 4 ->
    append(Rest93, [nth0(3, Xs, R)], Rest94) ; true),
    K94 is 4,
    (true , 4 =\= 4 ->
    append(Rest94, [nth0(4, Xs, R)], Rest95) ; true),
    K95 is 5,
    A15 = nth0(3, Xs, R),
    B15 = nth0(4, Xs, R),
    Op60 = OP_ADD,
    Node90 = map{tag: "Bin", op: Op60, left: A15, right: B15},
    (solve(append(Rest95, [map{tag: "Bin", op: Op60, left: A15, right: B15}], R)) ->
    Return93 = true ; true),
    Op61 = OP_SUB,
    Node91 = map{tag: "Bin", op: Op61, left: A15, right: B15},
    (solve(append(Rest95, [map{tag: "Bin", op: Op61, left: A15, right: B15}], R)) ->
    Return94 = true ; true),
    Op62 = OP_MUL,
    Node92 = map{tag: "Bin", op: Op62, left: A15, right: B15},
    (solve(append(Rest95, [map{tag: "Bin", op: Op62, left: A15, right: B15}], R)) ->
    Return95 = true ; true),
    Op63 = OP_DIV,
    Node93 = map{tag: "Bin", op: Op63, left: A15, right: B15},
    (solve(append(Rest95, [map{tag: "Bin", op: Op63, left: A15, right: B15}], R)) ->
    Return96 = true ; true),
    Node94 = map{tag: "Bin", op: OP_SUB, left: B15, right: A15},
    (solve(append(Rest95, [map{tag: "Bin", op: OP_SUB, left: B15, right: A15}], R)) ->
    Return97 = true ; true),
    Node95 = map{tag: "Bin", op: OP_DIV, left: B15, right: A15},
    (solve(append(Rest95, [map{tag: "Bin", op: OP_DIV, left: B15, right: A15}], R)) ->
    Return98 = true ; true),
    J19 is 5,
    Rest96 = [],
    K96 is 0,
    (true , 0 =\= 5 ->
    append([], [nth0(0, Xs, R)], Rest97) ; true),
    K97 is 1,
    (true , 1 =\= 5 ->
    append(Rest97, [nth0(1, Xs, R)], Rest98) ; true),
    K98 is 2,
    (true , 2 =\= 5 ->
    append(Rest98, [nth0(2, Xs, R)], Rest99) ; true),
    K99 is 3,
    (false , 3 =\= 5 ->
    append(Rest99, [nth0(3, Xs, R)], Rest100) ; true),
    K100 is 4,
    (true , 4 =\= 5 ->
    append(Rest100, [nth0(4, Xs, R)], Rest101) ; true),
    K101 is 5,
    A16 = nth0(3, Xs, R),
    B16 = nth0(5, Xs, R),
    Op64 = OP_ADD,
    Node96 = map{tag: "Bin", op: Op64, left: A16, right: B16},
    (solve(append(Rest101, [map{tag: "Bin", op: Op64, left: A16, right: B16}], R)) ->
    Return99 = true ; true),
    Op65 = OP_SUB,
    Node97 = map{tag: "Bin", op: Op65, left: A16, right: B16},
    (solve(append(Rest101, [map{tag: "Bin", op: Op65, left: A16, right: B16}], R)) ->
    Return100 = true ; true),
    Op66 = OP_MUL,
    Node98 = map{tag: "Bin", op: Op66, left: A16, right: B16},
    (solve(append(Rest101, [map{tag: "Bin", op: Op66, left: A16, right: B16}], R)) ->
    Return101 = true ; true),
    Op67 = OP_DIV,
    Node99 = map{tag: "Bin", op: Op67, left: A16, right: B16},
    (solve(append(Rest101, [map{tag: "Bin", op: Op67, left: A16, right: B16}], R)) ->
    Return102 = true ; true),
    Node100 = map{tag: "Bin", op: OP_SUB, left: B16, right: A16},
    (solve(append(Rest101, [map{tag: "Bin", op: OP_SUB, left: B16, right: A16}], R)) ->
    Return103 = true ; true),
    Node101 = map{tag: "Bin", op: OP_DIV, left: B16, right: A16},
    (solve(append(Rest101, [map{tag: "Bin", op: OP_DIV, left: B16, right: A16}], R)) ->
    Return104 = true ; true),
    J20 is 6,
    Rest102 = [],
    K102 is 0,
    (true , 0 =\= 6 ->
    append([], [nth0(0, Xs, R)], Rest103) ; true),
    K103 is 1,
    (true , 1 =\= 6 ->
    append(Rest103, [nth0(1, Xs, R)], Rest104) ; true),
    K104 is 2,
    (true , 2 =\= 6 ->
    append(Rest104, [nth0(2, Xs, R)], Rest105) ; true),
    K105 is 3,
    (false , 3 =\= 6 ->
    append(Rest105, [nth0(3, Xs, R)], Rest106) ; true),
    K106 is 4,
    (true , 4 =\= 6 ->
    append(Rest106, [nth0(4, Xs, R)], Rest107) ; true),
    K107 is 5,
    A17 = nth0(3, Xs, R),
    B17 = nth0(6, Xs, R),
    Op68 = OP_ADD,
    Node102 = map{tag: "Bin", op: Op68, left: A17, right: B17},
    (solve(append(Rest107, [map{tag: "Bin", op: Op68, left: A17, right: B17}], R)) ->
    Return105 = true ; true),
    Op69 = OP_SUB,
    Node103 = map{tag: "Bin", op: Op69, left: A17, right: B17},
    (solve(append(Rest107, [map{tag: "Bin", op: Op69, left: A17, right: B17}], R)) ->
    Return106 = true ; true),
    Op70 = OP_MUL,
    Node104 = map{tag: "Bin", op: Op70, left: A17, right: B17},
    (solve(append(Rest107, [map{tag: "Bin", op: Op70, left: A17, right: B17}], R)) ->
    Return107 = true ; true),
    Op71 = OP_DIV,
    Node105 = map{tag: "Bin", op: Op71, left: A17, right: B17},
    (solve(append(Rest107, [map{tag: "Bin", op: Op71, left: A17, right: B17}], R)) ->
    Return108 = true ; true),
    Node106 = map{tag: "Bin", op: OP_SUB, left: B17, right: A17},
    (solve(append(Rest107, [map{tag: "Bin", op: OP_SUB, left: B17, right: A17}], R)) ->
    Return109 = true ; true),
    Node107 = map{tag: "Bin", op: OP_DIV, left: B17, right: A17},
    (solve(append(Rest107, [map{tag: "Bin", op: OP_DIV, left: B17, right: A17}], R)) ->
    Return110 = true ; true),
    J21 is 7,
    Rest108 = [],
    K108 is 0,
    (true , 0 =\= 7 ->
    append([], [nth0(0, Xs, R)], Rest109) ; true),
    K109 is 1,
    (true , 1 =\= 7 ->
    append(Rest109, [nth0(1, Xs, R)], Rest110) ; true),
    K110 is 2,
    (true , 2 =\= 7 ->
    append(Rest110, [nth0(2, Xs, R)], Rest111) ; true),
    K111 is 3,
    (false , 3 =\= 7 ->
    append(Rest111, [nth0(3, Xs, R)], Rest112) ; true),
    K112 is 4,
    (true , 4 =\= 7 ->
    append(Rest112, [nth0(4, Xs, R)], Rest113) ; true),
    K113 is 5,
    A18 = nth0(3, Xs, R),
    B18 = nth0(7, Xs, R),
    Op72 = OP_ADD,
    Node108 = map{tag: "Bin", op: Op72, left: A18, right: B18},
    (solve(append(Rest113, [map{tag: "Bin", op: Op72, left: A18, right: B18}], R)) ->
    Return111 = true ; true),
    Op73 = OP_SUB,
    Node109 = map{tag: "Bin", op: Op73, left: A18, right: B18},
    (solve(append(Rest113, [map{tag: "Bin", op: Op73, left: A18, right: B18}], R)) ->
    Return112 = true ; true),
    Op74 = OP_MUL,
    Node110 = map{tag: "Bin", op: Op74, left: A18, right: B18},
    (solve(append(Rest113, [map{tag: "Bin", op: Op74, left: A18, right: B18}], R)) ->
    Return113 = true ; true),
    Op75 = OP_DIV,
    Node111 = map{tag: "Bin", op: Op75, left: A18, right: B18},
    (solve(append(Rest113, [map{tag: "Bin", op: Op75, left: A18, right: B18}], R)) ->
    Return114 = true ; true),
    Node112 = map{tag: "Bin", op: OP_SUB, left: B18, right: A18},
    (solve(append(Rest113, [map{tag: "Bin", op: OP_SUB, left: B18, right: A18}], R)) ->
    Return115 = true ; true),
    Node113 = map{tag: "Bin", op: OP_DIV, left: B18, right: A18},
    (solve(append(Rest113, [map{tag: "Bin", op: OP_DIV, left: B18, right: A18}], R)) ->
    Return116 = true ; true),
    J22 is 8,
    Rest114 = [],
    K114 is 0,
    (true , 0 =\= 8 ->
    append([], [nth0(0, Xs, R)], Rest115) ; true),
    K115 is 1,
    (true , 1 =\= 8 ->
    append(Rest115, [nth0(1, Xs, R)], Rest116) ; true),
    K116 is 2,
    (true , 2 =\= 8 ->
    append(Rest116, [nth0(2, Xs, R)], Rest117) ; true),
    K117 is 3,
    (false , 3 =\= 8 ->
    append(Rest117, [nth0(3, Xs, R)], Rest118) ; true),
    K118 is 4,
    (true , 4 =\= 8 ->
    append(Rest118, [nth0(4, Xs, R)], Rest119) ; true),
    K119 is 5,
    A19 = nth0(3, Xs, R),
    B19 = nth0(8, Xs, R),
    Op76 = OP_ADD,
    Node114 = map{tag: "Bin", op: Op76, left: A19, right: B19},
    (solve(append(Rest119, [map{tag: "Bin", op: Op76, left: A19, right: B19}], R)) ->
    Return117 = true ; true),
    Op77 = OP_SUB,
    Node115 = map{tag: "Bin", op: Op77, left: A19, right: B19},
    (solve(append(Rest119, [map{tag: "Bin", op: Op77, left: A19, right: B19}], R)) ->
    Return118 = true ; true),
    Op78 = OP_MUL,
    Node116 = map{tag: "Bin", op: Op78, left: A19, right: B19},
    (solve(append(Rest119, [map{tag: "Bin", op: Op78, left: A19, right: B19}], R)) ->
    Return119 = true ; true),
    Op79 = OP_DIV,
    Node117 = map{tag: "Bin", op: Op79, left: A19, right: B19},
    (solve(append(Rest119, [map{tag: "Bin", op: Op79, left: A19, right: B19}], R)) ->
    Return120 = true ; true),
    Node118 = map{tag: "Bin", op: OP_SUB, left: B19, right: A19},
    (solve(append(Rest119, [map{tag: "Bin", op: OP_SUB, left: B19, right: A19}], R)) ->
    Return121 = true ; true),
    Node119 = map{tag: "Bin", op: OP_DIV, left: B19, right: A19},
    (solve(append(Rest119, [map{tag: "Bin", op: OP_DIV, left: B19, right: A19}], R)) ->
    Return122 = true ; true),
    J23 is 9,
    I4 is 4,
    J24 is 5,
    Rest120 = [],
    K120 is 0,
    (true , 0 =\= 5 ->
    append([], [nth0(0, Xs, R)], Rest121) ; true),
    K121 is 1,
    (true , 1 =\= 5 ->
    append(Rest121, [nth0(1, Xs, R)], Rest122) ; true),
    K122 is 2,
    (true , 2 =\= 5 ->
    append(Rest122, [nth0(2, Xs, R)], Rest123) ; true),
    K123 is 3,
    (true , 3 =\= 5 ->
    append(Rest123, [nth0(3, Xs, R)], Rest124) ; true),
    K124 is 4,
    (false , 4 =\= 5 ->
    append(Rest124, [nth0(4, Xs, R)], Rest125) ; true),
    K125 is 5,
    A20 = nth0(4, Xs, R),
    B20 = nth0(5, Xs, R),
    Op80 = OP_ADD,
    Node120 = map{tag: "Bin", op: Op80, left: A20, right: B20},
    (solve(append(Rest125, [map{tag: "Bin", op: Op80, left: A20, right: B20}], R)) ->
    Return123 = true ; true),
    Op81 = OP_SUB,
    Node121 = map{tag: "Bin", op: Op81, left: A20, right: B20},
    (solve(append(Rest125, [map{tag: "Bin", op: Op81, left: A20, right: B20}], R)) ->
    Return124 = true ; true),
    Op82 = OP_MUL,
    Node122 = map{tag: "Bin", op: Op82, left: A20, right: B20},
    (solve(append(Rest125, [map{tag: "Bin", op: Op82, left: A20, right: B20}], R)) ->
    Return125 = true ; true),
    Op83 = OP_DIV,
    Node123 = map{tag: "Bin", op: Op83, left: A20, right: B20},
    (solve(append(Rest125, [map{tag: "Bin", op: Op83, left: A20, right: B20}], R)) ->
    Return126 = true ; true),
    Node124 = map{tag: "Bin", op: OP_SUB, left: B20, right: A20},
    (solve(append(Rest125, [map{tag: "Bin", op: OP_SUB, left: B20, right: A20}], R)) ->
    Return127 = true ; true),
    Node125 = map{tag: "Bin", op: OP_DIV, left: B20, right: A20},
    (solve(append(Rest125, [map{tag: "Bin", op: OP_DIV, left: B20, right: A20}], R)) ->
    Return128 = true ; true),
    J25 is 6,
    Rest126 = [],
    K126 is 0,
    (true , 0 =\= 6 ->
    append([], [nth0(0, Xs, R)], Rest127) ; true),
    K127 is 1,
    (true , 1 =\= 6 ->
    append(Rest127, [nth0(1, Xs, R)], Rest128) ; true),
    K128 is 2,
    (true , 2 =\= 6 ->
    append(Rest128, [nth0(2, Xs, R)], Rest129) ; true),
    K129 is 3,
    (true , 3 =\= 6 ->
    append(Rest129, [nth0(3, Xs, R)], Rest130) ; true),
    K130 is 4,
    (false , 4 =\= 6 ->
    append(Rest130, [nth0(4, Xs, R)], Rest131) ; true),
    K131 is 5,
    A21 = nth0(4, Xs, R),
    B21 = nth0(6, Xs, R),
    Op84 = OP_ADD,
    Node126 = map{tag: "Bin", op: Op84, left: A21, right: B21},
    (solve(append(Rest131, [map{tag: "Bin", op: Op84, left: A21, right: B21}], R)) ->
    Return129 = true ; true),
    Op85 = OP_SUB,
    Node127 = map{tag: "Bin", op: Op85, left: A21, right: B21},
    (solve(append(Rest131, [map{tag: "Bin", op: Op85, left: A21, right: B21}], R)) ->
    Return130 = true ; true),
    Op86 = OP_MUL,
    Node128 = map{tag: "Bin", op: Op86, left: A21, right: B21},
    (solve(append(Rest131, [map{tag: "Bin", op: Op86, left: A21, right: B21}], R)) ->
    Return131 = true ; true),
    Op87 = OP_DIV,
    Node129 = map{tag: "Bin", op: Op87, left: A21, right: B21},
    (solve(append(Rest131, [map{tag: "Bin", op: Op87, left: A21, right: B21}], R)) ->
    Return132 = true ; true),
    Node130 = map{tag: "Bin", op: OP_SUB, left: B21, right: A21},
    (solve(append(Rest131, [map{tag: "Bin", op: OP_SUB, left: B21, right: A21}], R)) ->
    Return133 = true ; true),
    Node131 = map{tag: "Bin", op: OP_DIV, left: B21, right: A21},
    (solve(append(Rest131, [map{tag: "Bin", op: OP_DIV, left: B21, right: A21}], R)) ->
    Return134 = true ; true),
    J26 is 7,
    Rest132 = [],
    K132 is 0,
    (true , 0 =\= 7 ->
    append([], [nth0(0, Xs, R)], Rest133) ; true),
    K133 is 1,
    (true , 1 =\= 7 ->
    append(Rest133, [nth0(1, Xs, R)], Rest134) ; true),
    K134 is 2,
    (true , 2 =\= 7 ->
    append(Rest134, [nth0(2, Xs, R)], Rest135) ; true),
    K135 is 3,
    (true , 3 =\= 7 ->
    append(Rest135, [nth0(3, Xs, R)], Rest136) ; true),
    K136 is 4,
    (false , 4 =\= 7 ->
    append(Rest136, [nth0(4, Xs, R)], Rest137) ; true),
    K137 is 5,
    A22 = nth0(4, Xs, R),
    B22 = nth0(7, Xs, R),
    Op88 = OP_ADD,
    Node132 = map{tag: "Bin", op: Op88, left: A22, right: B22},
    (solve(append(Rest137, [map{tag: "Bin", op: Op88, left: A22, right: B22}], R)) ->
    Return135 = true ; true),
    Op89 = OP_SUB,
    Node133 = map{tag: "Bin", op: Op89, left: A22, right: B22},
    (solve(append(Rest137, [map{tag: "Bin", op: Op89, left: A22, right: B22}], R)) ->
    Return136 = true ; true),
    Op90 = OP_MUL,
    Node134 = map{tag: "Bin", op: Op90, left: A22, right: B22},
    (solve(append(Rest137, [map{tag: "Bin", op: Op90, left: A22, right: B22}], R)) ->
    Return137 = true ; true),
    Op91 = OP_DIV,
    Node135 = map{tag: "Bin", op: Op91, left: A22, right: B22},
    (solve(append(Rest137, [map{tag: "Bin", op: Op91, left: A22, right: B22}], R)) ->
    Return138 = true ; true),
    Node136 = map{tag: "Bin", op: OP_SUB, left: B22, right: A22},
    (solve(append(Rest137, [map{tag: "Bin", op: OP_SUB, left: B22, right: A22}], R)) ->
    Return139 = true ; true),
    Node137 = map{tag: "Bin", op: OP_DIV, left: B22, right: A22},
    (solve(append(Rest137, [map{tag: "Bin", op: OP_DIV, left: B22, right: A22}], R)) ->
    Return140 = true ; true),
    J27 is 8,
    Rest138 = [],
    K138 is 0,
    (true , 0 =\= 8 ->
    append([], [nth0(0, Xs, R)], Rest139) ; true),
    K139 is 1,
    (true , 1 =\= 8 ->
    append(Rest139, [nth0(1, Xs, R)], Rest140) ; true),
    K140 is 2,
    (true , 2 =\= 8 ->
    append(Rest140, [nth0(2, Xs, R)], Rest141) ; true),
    K141 is 3,
    (true , 3 =\= 8 ->
    append(Rest141, [nth0(3, Xs, R)], Rest142) ; true),
    K142 is 4,
    (false , 4 =\= 8 ->
    append(Rest142, [nth0(4, Xs, R)], Rest143) ; true),
    K143 is 5,
    A23 = nth0(4, Xs, R),
    B23 = nth0(8, Xs, R),
    Op92 = OP_ADD,
    Node138 = map{tag: "Bin", op: Op92, left: A23, right: B23},
    (solve(append(Rest143, [map{tag: "Bin", op: Op92, left: A23, right: B23}], R)) ->
    Return141 = true ; true),
    Op93 = OP_SUB,
    Node139 = map{tag: "Bin", op: Op93, left: A23, right: B23},
    (solve(append(Rest143, [map{tag: "Bin", op: Op93, left: A23, right: B23}], R)) ->
    Return142 = true ; true),
    Op94 = OP_MUL,
    Node140 = map{tag: "Bin", op: Op94, left: A23, right: B23},
    (solve(append(Rest143, [map{tag: "Bin", op: Op94, left: A23, right: B23}], R)) ->
    Return143 = true ; true),
    Op95 = OP_DIV,
    Node141 = map{tag: "Bin", op: Op95, left: A23, right: B23},
    (solve(append(Rest143, [map{tag: "Bin", op: Op95, left: A23, right: B23}], R)) ->
    Return144 = true ; true),
    Node142 = map{tag: "Bin", op: OP_SUB, left: B23, right: A23},
    (solve(append(Rest143, [map{tag: "Bin", op: OP_SUB, left: B23, right: A23}], R)) ->
    Return145 = true ; true),
    Node143 = map{tag: "Bin", op: OP_DIV, left: B23, right: A23},
    (solve(append(Rest143, [map{tag: "Bin", op: OP_DIV, left: B23, right: A23}], R)) ->
    Return146 = true ; true),
    J28 is 9,
    Rest144 = [],
    K144 is 0,
    (true , 0 =\= 9 ->
    append([], [nth0(0, Xs, R)], Rest145) ; true),
    K145 is 1,
    (true , 1 =\= 9 ->
    append(Rest145, [nth0(1, Xs, R)], Rest146) ; true),
    K146 is 2,
    (true , 2 =\= 9 ->
    append(Rest146, [nth0(2, Xs, R)], Rest147) ; true),
    K147 is 3,
    (true , 3 =\= 9 ->
    append(Rest147, [nth0(3, Xs, R)], Rest148) ; true),
    K148 is 4,
    (false , 4 =\= 9 ->
    append(Rest148, [nth0(4, Xs, R)], Rest149) ; true),
    K149 is 5,
    A24 = nth0(4, Xs, R),
    B24 = nth0(9, Xs, R),
    Op96 = OP_ADD,
    Node144 = map{tag: "Bin", op: Op96, left: A24, right: B24},
    (solve(append(Rest149, [map{tag: "Bin", op: Op96, left: A24, right: B24}], R)) ->
    Return147 = true ; true),
    Op97 = OP_SUB,
    Node145 = map{tag: "Bin", op: Op97, left: A24, right: B24},
    (solve(append(Rest149, [map{tag: "Bin", op: Op97, left: A24, right: B24}], R)) ->
    Return148 = true ; true),
    Op98 = OP_MUL,
    Node146 = map{tag: "Bin", op: Op98, left: A24, right: B24},
    (solve(append(Rest149, [map{tag: "Bin", op: Op98, left: A24, right: B24}], R)) ->
    Return149 = true ; true),
    Op99 = OP_DIV,
    Node147 = map{tag: "Bin", op: Op99, left: A24, right: B24},
    (solve(append(Rest149, [map{tag: "Bin", op: Op99, left: A24, right: B24}], R)) ->
    Return150 = true ; true),
    Node148 = map{tag: "Bin", op: OP_SUB, left: B24, right: A24},
    (solve(append(Rest149, [map{tag: "Bin", op: OP_SUB, left: B24, right: A24}], R)) ->
    Return151 = true ; true),
    Node149 = map{tag: "Bin", op: OP_DIV, left: B24, right: A24},
    (solve(append(Rest149, [map{tag: "Bin", op: OP_DIV, left: B24, right: A24}], R)) ->
    Return152 = true ; true),
    J29 is 10,
    I5 is 5,
    Return153 = false,
    R = Return153.

main(R) :-
    Iter is 0,
    Iter1 is 0,
    Cards = [],
    I is 0,
    N is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N)], Cards1),
    writeln((string_concat(" ", N, T), T)),
    I1 is 1,
    N1 is (now() mod (Digit_range - 1)) + 1,
    append(Cards1, [newNum(N1)], Cards2),
    writeln((string_concat(" ", N1, T), T)),
    I2 is 2,
    N2 is (now() mod (Digit_range - 1)) + 1,
    append(Cards2, [newNum(N2)], Cards3),
    writeln((string_concat(" ", N2, T), T)),
    I3 is 3,
    N3 is (now() mod (Digit_range - 1)) + 1,
    append(Cards3, [newNum(N3)], Cards4),
    writeln((string_concat(" ", N3, T), T)),
    I4 is 4,
    N4 is (now() mod (Digit_range - 1)) + 1,
    append(Cards4, [newNum(N4)], Cards5),
    writeln((string_concat(" ", N4, T), T)),
    I5 is 5,
    writeln(":  "),
    (\+(solve(Cards5)) ->
    writeln("No solution") ; true),
    Iter2 is 1,
    Cards6 = [],
    I6 is 0,
    N5 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N5)], Cards7),
    writeln((string_concat(" ", N5, T), T)),
    I7 is 1,
    N6 is (now() mod (Digit_range - 1)) + 1,
    append(Cards7, [newNum(N6)], Cards8),
    writeln((string_concat(" ", N6, T), T)),
    I8 is 2,
    N7 is (now() mod (Digit_range - 1)) + 1,
    append(Cards8, [newNum(N7)], Cards9),
    writeln((string_concat(" ", N7, T), T)),
    I9 is 3,
    N8 is (now() mod (Digit_range - 1)) + 1,
    append(Cards9, [newNum(N8)], Cards10),
    writeln((string_concat(" ", N8, T), T)),
    I10 is 4,
    N9 is (now() mod (Digit_range - 1)) + 1,
    append(Cards10, [newNum(N9)], Cards11),
    writeln((string_concat(" ", N9, T), T)),
    I11 is 5,
    writeln(":  "),
    (\+(solve(Cards11)) ->
    writeln("No solution") ; true),
    Iter3 is 2,
    Cards12 = [],
    I12 is 0,
    N10 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N10)], Cards13),
    writeln((string_concat(" ", N10, T), T)),
    I13 is 1,
    N11 is (now() mod (Digit_range - 1)) + 1,
    append(Cards13, [newNum(N11)], Cards14),
    writeln((string_concat(" ", N11, T), T)),
    I14 is 2,
    N12 is (now() mod (Digit_range - 1)) + 1,
    append(Cards14, [newNum(N12)], Cards15),
    writeln((string_concat(" ", N12, T), T)),
    I15 is 3,
    N13 is (now() mod (Digit_range - 1)) + 1,
    append(Cards15, [newNum(N13)], Cards16),
    writeln((string_concat(" ", N13, T), T)),
    I16 is 4,
    N14 is (now() mod (Digit_range - 1)) + 1,
    append(Cards16, [newNum(N14)], Cards17),
    writeln((string_concat(" ", N14, T), T)),
    I17 is 5,
    writeln(":  "),
    (\+(solve(Cards17)) ->
    writeln("No solution") ; true),
    Iter4 is 3,
    Cards18 = [],
    I18 is 0,
    N15 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N15)], Cards19),
    writeln((string_concat(" ", N15, T), T)),
    I19 is 1,
    N16 is (now() mod (Digit_range - 1)) + 1,
    append(Cards19, [newNum(N16)], Cards20),
    writeln((string_concat(" ", N16, T), T)),
    I20 is 2,
    N17 is (now() mod (Digit_range - 1)) + 1,
    append(Cards20, [newNum(N17)], Cards21),
    writeln((string_concat(" ", N17, T), T)),
    I21 is 3,
    N18 is (now() mod (Digit_range - 1)) + 1,
    append(Cards21, [newNum(N18)], Cards22),
    writeln((string_concat(" ", N18, T), T)),
    I22 is 4,
    N19 is (now() mod (Digit_range - 1)) + 1,
    append(Cards22, [newNum(N19)], Cards23),
    writeln((string_concat(" ", N19, T), T)),
    I23 is 5,
    writeln(":  "),
    (\+(solve(Cards23)) ->
    writeln("No solution") ; true),
    Iter5 is 4,
    Cards24 = [],
    I24 is 0,
    N20 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N20)], Cards25),
    writeln((string_concat(" ", N20, T), T)),
    I25 is 1,
    N21 is (now() mod (Digit_range - 1)) + 1,
    append(Cards25, [newNum(N21)], Cards26),
    writeln((string_concat(" ", N21, T), T)),
    I26 is 2,
    N22 is (now() mod (Digit_range - 1)) + 1,
    append(Cards26, [newNum(N22)], Cards27),
    writeln((string_concat(" ", N22, T), T)),
    I27 is 3,
    N23 is (now() mod (Digit_range - 1)) + 1,
    append(Cards27, [newNum(N23)], Cards28),
    writeln((string_concat(" ", N23, T), T)),
    I28 is 4,
    N24 is (now() mod (Digit_range - 1)) + 1,
    append(Cards28, [newNum(N24)], Cards29),
    writeln((string_concat(" ", N24, T), T)),
    I29 is 5,
    writeln(":  "),
    (\+(solve(Cards29)) ->
    writeln("No solution") ; true),
    Iter6 is 5,
    Cards30 = [],
    I30 is 0,
    N25 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N25)], Cards31),
    writeln((string_concat(" ", N25, T), T)),
    I31 is 1,
    N26 is (now() mod (Digit_range - 1)) + 1,
    append(Cards31, [newNum(N26)], Cards32),
    writeln((string_concat(" ", N26, T), T)),
    I32 is 2,
    N27 is (now() mod (Digit_range - 1)) + 1,
    append(Cards32, [newNum(N27)], Cards33),
    writeln((string_concat(" ", N27, T), T)),
    I33 is 3,
    N28 is (now() mod (Digit_range - 1)) + 1,
    append(Cards33, [newNum(N28)], Cards34),
    writeln((string_concat(" ", N28, T), T)),
    I34 is 4,
    N29 is (now() mod (Digit_range - 1)) + 1,
    append(Cards34, [newNum(N29)], Cards35),
    writeln((string_concat(" ", N29, T), T)),
    I35 is 5,
    writeln(":  "),
    (\+(solve(Cards35)) ->
    writeln("No solution") ; true),
    Iter7 is 6,
    Cards36 = [],
    I36 is 0,
    N30 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N30)], Cards37),
    writeln((string_concat(" ", N30, T), T)),
    I37 is 1,
    N31 is (now() mod (Digit_range - 1)) + 1,
    append(Cards37, [newNum(N31)], Cards38),
    writeln((string_concat(" ", N31, T), T)),
    I38 is 2,
    N32 is (now() mod (Digit_range - 1)) + 1,
    append(Cards38, [newNum(N32)], Cards39),
    writeln((string_concat(" ", N32, T), T)),
    I39 is 3,
    N33 is (now() mod (Digit_range - 1)) + 1,
    append(Cards39, [newNum(N33)], Cards40),
    writeln((string_concat(" ", N33, T), T)),
    I40 is 4,
    N34 is (now() mod (Digit_range - 1)) + 1,
    append(Cards40, [newNum(N34)], Cards41),
    writeln((string_concat(" ", N34, T), T)),
    I41 is 5,
    writeln(":  "),
    (\+(solve(Cards41)) ->
    writeln("No solution") ; true),
    Iter8 is 7,
    Cards42 = [],
    I42 is 0,
    N35 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N35)], Cards43),
    writeln((string_concat(" ", N35, T), T)),
    I43 is 1,
    N36 is (now() mod (Digit_range - 1)) + 1,
    append(Cards43, [newNum(N36)], Cards44),
    writeln((string_concat(" ", N36, T), T)),
    I44 is 2,
    N37 is (now() mod (Digit_range - 1)) + 1,
    append(Cards44, [newNum(N37)], Cards45),
    writeln((string_concat(" ", N37, T), T)),
    I45 is 3,
    N38 is (now() mod (Digit_range - 1)) + 1,
    append(Cards45, [newNum(N38)], Cards46),
    writeln((string_concat(" ", N38, T), T)),
    I46 is 4,
    N39 is (now() mod (Digit_range - 1)) + 1,
    append(Cards46, [newNum(N39)], Cards47),
    writeln((string_concat(" ", N39, T), T)),
    I47 is 5,
    writeln(":  "),
    (\+(solve(Cards47)) ->
    writeln("No solution") ; true),
    Iter9 is 8,
    Cards48 = [],
    I48 is 0,
    N40 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N40)], Cards49),
    writeln((string_concat(" ", N40, T), T)),
    I49 is 1,
    N41 is (now() mod (Digit_range - 1)) + 1,
    append(Cards49, [newNum(N41)], Cards50),
    writeln((string_concat(" ", N41, T), T)),
    I50 is 2,
    N42 is (now() mod (Digit_range - 1)) + 1,
    append(Cards50, [newNum(N42)], Cards51),
    writeln((string_concat(" ", N42, T), T)),
    I51 is 3,
    N43 is (now() mod (Digit_range - 1)) + 1,
    append(Cards51, [newNum(N43)], Cards52),
    writeln((string_concat(" ", N43, T), T)),
    I52 is 4,
    N44 is (now() mod (Digit_range - 1)) + 1,
    append(Cards52, [newNum(N44)], Cards53),
    writeln((string_concat(" ", N44, T), T)),
    I53 is 5,
    writeln(":  "),
    (\+(solve(Cards53)) ->
    writeln("No solution") ; true),
    Iter10 is 9,
    Cards54 = [],
    I54 is 0,
    N45 is (now() mod (Digit_range - 1)) + 1,
    append([], [newNum(N45)], Cards55),
    writeln((string_concat(" ", N45, T), T)),
    I55 is 1,
    N46 is (now() mod (Digit_range - 1)) + 1,
    append(Cards55, [newNum(N46)], Cards56),
    writeln((string_concat(" ", N46, T), T)),
    I56 is 2,
    N47 is (now() mod (Digit_range - 1)) + 1,
    append(Cards56, [newNum(N47)], Cards57),
    writeln((string_concat(" ", N47, T), T)),
    I57 is 3,
    N48 is (now() mod (Digit_range - 1)) + 1,
    append(Cards57, [newNum(N48)], Cards58),
    writeln((string_concat(" ", N48, T), T)),
    I58 is 4,
    N49 is (now() mod (Digit_range - 1)) + 1,
    append(Cards58, [newNum(N49)], Cards59),
    writeln((string_concat(" ", N49, T), T)),
    I59 is 5,
    writeln(":  "),
    (\+(solve(Cards59)) ->
    writeln("No solution") ; true),
    R = Return.

main :-
    OP_ADD is 1,
    OP_SUB is 2,
    OP_MUL is 3,
    OP_DIV is 4,
    N_cards is 4,
    Goal is 24,
    Digit_range is 9,
    main(_).
