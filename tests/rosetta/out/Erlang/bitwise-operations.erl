% bitwise-operations.erl - generated from bitwise-operations.mochi

toUnsigned16(N) ->
    U0 = N,
    (case (U0 < 0) of true -> U1 = (U0 + 65536); _ -> ok end),
    rem(U1, 65536).

bin16(N) ->
    U2 = toUnsigned16(N),
    Bits0 = "",
    Mask0 = 32768,
    {Mask1} = lists:foldl(fun(I, {Mask}) -> (case (U2 >= Mask) of true -> Bits1 = Bits0 ++ "1", U3 = (U2 - Mask); _ -> Bits2 = Bits1 ++ "0" end), Mask1 = ((Mask / 2)), {Mask1} end, {Mask0}, lists:seq(0, (16)-1)),
    Bits2.

bit_and(A, B) ->
    Ua0 = toUnsigned16(A),
    Ub0 = toUnsigned16(B),
    Res0 = 0,
    Bit0 = 1,
    {Ua1, Ub1, Bit1} = lists:foldl(fun(I, {Ua, Ub, Bit}) -> (case ((rem(Ua, 2) == 1) andalso (rem(Ub, 2) == 1)) of true -> Res1 = (Res0 + Bit); _ -> ok end), Ua1 = ((Ua / 2)), Ub1 = ((Ub / 2)), Bit1 = (Bit * 2), {Ua1, Ub1, Bit1} end, {Ua0, Ub0, Bit0}, lists:seq(0, (16)-1)),
    Res1.

bit_or(A, B) ->
    Ua2 = toUnsigned16(A),
    Ub2 = toUnsigned16(B),
    Res2 = 0,
    Bit2 = 1,
    {Ua3, Ub3, Bit3} = lists:foldl(fun(I, {Ua, Ub, Bit}) -> (case ((rem(Ua, 2) == 1) orelse (rem(Ub, 2) == 1)) of true -> Res3 = (Res2 + Bit); _ -> ok end), Ua3 = ((Ua / 2)), Ub3 = ((Ub / 2)), Bit3 = (Bit * 2), {Ua3, Ub3, Bit3} end, {Ua2, Ub2, Bit2}, lists:seq(0, (16)-1)),
    Res3.

bit_xor(A, B) ->
    Ua4 = toUnsigned16(A),
    Ub4 = toUnsigned16(B),
    Res4 = 0,
    Bit4 = 1,
    {Ua5, Ub5, Bit5} = lists:foldl(fun(I, {Ua, Ub, Bit}) -> Abit = rem(Ua, 2), Bbit = rem(Ub, 2), (case ((((Abit == 1) andalso (Bbit == 0))) orelse (((Abit == 0) andalso (Bbit == 1)))) of true -> Res5 = (Res4 + Bit); _ -> ok end), Ua5 = ((Ua / 2)), Ub5 = ((Ub / 2)), Bit5 = (Bit * 2), {Ua5, Ub5, Bit5} end, {Ua4, Ub4, Bit4}, lists:seq(0, (16)-1)),
    Res5.

bit_not(A) ->
    Ua6 = toUnsigned16(A),
    (65535 - Ua6).

shl(A, B) ->
    Ua7 = toUnsigned16(A),
    I0 = 0,
    (fun Loop0(Ua, I) -> case (I < B) of true -> Ua8 = rem(((Ua * 2)), 65536), I1 = (I + 1), Loop0(Ua8, I1); _ -> ok end end(Ua7, I0)),
    Ua8.

shr(A, B) ->
    Ua9 = toUnsigned16(A),
    I2 = 0,
    (fun Loop1(Ua, I) -> case (I < B) of true -> Ua10 = ((Ua / 2)), I3 = (I + 1), Loop1(I3, Ua10); _ -> ok end end(Ua9, I2)),
    Ua10.

las(A, B) ->
    shl(A, B).

ras(A, B) ->
    Val0 = A,
    I4 = 0,
    (fun Loop2(I) -> case (I < B) of true -> (case (Val0 >= 0) of true -> Val1 = ((Val0 / 2)); _ -> Val2 = ((((Val1 - 1)) / 2)) end), I5 = (I + 1), Loop2(I5); _ -> ok end end(I4)),
    toUnsigned16(Val2).

rol(A, B) ->
    Ua11 = toUnsigned16(A),
    Left = shl(Ua11, B),
    Right = shr(Ua11, (16 - B)),
    toUnsigned16((Left + Right)).

ror(A, B) ->
    Ua12 = toUnsigned16(A),
    Right = shr(Ua12, B),
    Left = shl(Ua12, (16 - B)),
    toUnsigned16((Left + Right)).

bitwise(A, B) ->
    io:format("~p~n", ["a:   " ++ bin16(A)]),
    io:format("~p~n", ["b:   " ++ bin16(B)]),
    io:format("~p~n", ["and: " ++ bin16(bit_and(A, B))]),
    io:format("~p~n", ["or:  " ++ bin16(bit_or(A, B))]),
    io:format("~p~n", ["xor: " ++ bin16(bit_xor(A, B))]),
    io:format("~p~n", ["not: " ++ bin16(bit_not(A))]),
    (case (B < 0) of true -> io:format("~p~n", ["Right operand is negative, but all shifts require an unsigned right operand (shift distance)."]), undefined; _ -> ok end),
    io:format("~p~n", ["shl: " ++ bin16(shl(A, B))]),
    io:format("~p~n", ["shr: " ++ bin16(shr(A, B))]),
    io:format("~p~n", ["las: " ++ bin16(las(A, B))]),
    io:format("~p~n", ["ras: " ++ bin16(ras(A, B))]),
    io:format("~p~n", ["rol: " ++ bin16(rol(A, B))]),
    io:format("~p~n", ["ror: " ++ bin16(ror(A, B))]).

main(_) ->
    bitwise(-460, 6).
