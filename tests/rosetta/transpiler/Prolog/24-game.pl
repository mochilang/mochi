:- initialization(main).
:- style_check(-singleton).

main(R) :-
    Digits = [],
    I is 0,
    append([], [(now() mod 9) + 1], Digits1),
    I1 is 1,
    append(Digits1, [(now() mod 9) + 1], Digits2),
    I2 is 2,
    append(Digits2, [(now() mod 9) + 1], Digits3),
    I3 is 3,
    append(Digits3, [(now() mod 9) + 1], Digits4),
    Numstr = "",
    I4 is 0,
    Numstr1 = (string_concat("", nth0(0, Digits4, R), T), T),
    I5 is 1,
    Numstr2 = (string_concat(Numstr1, nth0(1, Digits4, R), T), T),
    I6 is 2,
    Numstr3 = (string_concat(Numstr2, nth0(2, Digits4, R), T), T),
    I7 is 3,
    Numstr4 = (string_concat(Numstr3, nth0(3, Digits4, R), T), T),
    writeln((string_concat((string_concat("Your numbers: ", Numstr4, T), T), "\n", T), T)),
    writeln("Enter RPN: "),
    input(Expr),
    (len(Expr) =\= 7 ->
    writeln("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)"),
    Return1 is 0 ; true),
    Stack = [],
    I8 is 0,
    Valid = true,
    Ch = (Len is 1 - 0, sub_string(Expr, 0, Len, _, R)),
    (Ch @>= "0" , Ch @=< "9" ->
true ; true),
    R = Return3.

main :-
    main(_).
