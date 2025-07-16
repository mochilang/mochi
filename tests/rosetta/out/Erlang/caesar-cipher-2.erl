% caesar-cipher-2.erl - generated from caesar-cipher-2.mochi

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

ord(Ch) ->
    Idx0 = indexOf(Upper, Ch),
    (case (Idx0 >= 0) of true -> (65 + Idx0); _ -> ok end),
    Idx1 = indexOf(Lower, Ch),
    (case (Idx1 >= 0) of true -> (97 + Idx1); _ -> ok end),
    0.

chr(N) ->
    (case ((N >= 65) andalso (N < 91)) of true -> lists:sublist(Upper, ((N - 65))+1, ((N - 64))-((N - 65))); _ -> ok end),
    (case ((N >= 97) andalso (N < 123)) of true -> lists:sublist(Lower, ((N - 97))+1, ((N - 96))-((N - 97))); _ -> ok end),
    "?".

shiftRune(R, K) ->
    (case ((R >= "a") andalso (R =< "z")) of true -> chr(((rem((((ord(R) - 97) + K)), 26)) + 97)); _ -> ok end),
    (case ((R >= "A") andalso (R =< "Z")) of true -> chr(((rem((((ord(R) - 65) + K)), 26)) + 65)); _ -> ok end),
    R.

encipher(S, K) ->
    Out0 = "",
    I2 = 0,
    (fun Loop1(Out, I) -> case (I < length(S)) of true -> Out1 = (Out + shiftRune(string:substr(S, (I)+1, ((I + 1))-(I)), K)), I3 = (I + 1), Loop1(Out1, I3); _ -> ok end end(Out0, I2)),
    Out1.

decipher(S, K) ->
    encipher(S, rem(((26 - rem(K, 26))), 26)).

main() ->
    io:format("~p~n", ["Plaintext: " ++ Pt]),
    try lists:foreach(fun(Key) -> try (case ((Key < 1) orelse (Key > 25)) of true -> io:format("~p~n", ["Key " ++ lists:flatten(io_lib:format("~p", [Key])) ++ " invalid"]), throw(continue); _ -> ok end), Ct = encipher(Pt, Key), io:format("~p~n", ["Key " ++ lists:flatten(io_lib:format("~p", [Key]))]), io:format("~p~n", ["  Enciphered: " ++ Ct]), io:format("~p~n", ["  Deciphered: " ++ decipher(Ct, Key)]) catch throw:continue -> ok end end, [0, 1, 7, 25, 26]) catch throw:break -> ok end.

main(_) ->
    main().
