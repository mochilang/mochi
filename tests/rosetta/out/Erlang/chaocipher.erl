% chaocipher.erl - generated from chaocipher.mochi

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

rotate(S, N) ->
    (string:substr(S, (N)+1, (length(S))-(N)) + string:substr(S, (0)+1, (N)-(0))).

scrambleLeft(S) ->
    (((string:substr(S, (0)+1, (1)-(0)) + string:substr(S, (2)+1, (14)-(2))) + string:substr(S, (1)+1, (2)-(1))) + string:substr(S, (14)+1, (length(S))-(14))).

scrambleRight(S) ->
    ((((string:substr(S, (1)+1, (3)-(1)) + string:substr(S, (4)+1, (15)-(4))) + string:substr(S, (3)+1, (4)-(3))) + string:substr(S, (15)+1, (length(S))-(15))) + string:substr(S, (0)+1, (1)-(0))).

chao(Text, Encode) ->
    Left0 = "HXUCZVAMDSLKPEFJRIGTWOBNYQ",
    Right0 = "PTLNBQDEOYSFAVZKGJRIHWXUMC",
    Out0 = "",
    I2 = 0,
    (fun Loop1(I, Left, Right) -> case (I < length(Text)) of true -> Ch = string:substr(Text, (I)+1, ((I + 1))-(I)), Idx0 = 0, (case Encode of undefined -> Idx2 = indexOf(Left, Ch), Out2 = (Out1 + string:substr(Right, (Idx2)+1, ((Idx2 + 1))-(Idx2))); false -> Idx2 = indexOf(Left, Ch), Out2 = (Out1 + string:substr(Right, (Idx2)+1, ((Idx2 + 1))-(Idx2))); _ -> Idx1 = indexOf(Right, Ch), Out1 = (Out0 + string:substr(Left, (Idx1)+1, ((Idx1 + 1))-(Idx1))) end), Left1 = rotate(Left, Idx2), Right1 = rotate(Right, Idx2), Left2 = scrambleLeft(Left), Right2 = scrambleRight(Right), I3 = (I + 1), Loop1(Left2, Right2, I3); _ -> ok end end(I2, Left0, Right0)),
    Out2.

main() ->
    Cipher = chao(Plain, true),
    io:format("~p~n", [Plain]),
    io:format("~p~n", [Cipher]),
    io:format("~p~n", [chao(Cipher, false)]).

main(_) ->
    main().
