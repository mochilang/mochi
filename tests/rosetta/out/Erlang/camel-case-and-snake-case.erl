% camel-case-and-snake-case.erl - generated from camel-case-and-snake-case.mochi

trimSpace(S) ->
    Start0 = 0,
    (fun Loop0(Start) -> case ((Start < length(S)) andalso (string:substr(S, (Start)+1, ((Start + 1))-(Start)) == " ")) of true -> Start1 = (Start + 1), Loop0(Start1); _ -> ok end end(Start0)),
    End0 = length(S),
    (fun Loop1(End) -> case ((End > Start1) andalso (string:substr(S, ((End - 1))+1, (End)-((End - 1))) == " ")) of true -> End1 = (End - 1), Loop1(End1); _ -> ok end end(End0)),
    string:substr(S, (Start1)+1, (End1)-(Start1)).

isUpper(Ch) ->
    ((Ch >= "A") andalso (Ch =< "Z")).

padLeft(S, W) ->
    Res0 = "",
    N0 = (W - length(S)),
    (fun Loop2(Res, N) -> case (N > 0) of true -> Res1 = Res ++ " ", N1 = (N - 1), Loop2(Res1, N1); _ -> ok end end(Res0, N0)),
    (Res1 + S).

snakeToCamel(S) ->
    S0 = trimSpace(S),
    Out0 = "",
    Up0 = false,
    I0 = 0,
    (fun Loop3(I) -> case (I < length(S0)) of true -> Ch = string:substr(S0, (I)+1, ((I + 1))-(I)), (case ((((Ch == "_") orelse (Ch == "-")) orelse (Ch == " ")) orelse (Ch == ".")) of true -> Up1 = true, I1 = (I + 1), throw(continue); _ -> ok end), (case (I == 0) of true -> Out1 = (Out0 + lower(Ch)), Up2 = false, I2 = (I + 1), throw(continue); _ -> ok end), (case Up2 of undefined -> Out3 = (Out2 + Ch); false -> Out3 = (Out2 + Ch); _ -> Out2 = (Out1 + upper(Ch)), Up3 = false end), I3 = (I + 1), Loop3(I3); _ -> ok end end(I0)),
    Out3.

camelToSnake(S) ->
    S1 = trimSpace(S0),
    Out4 = "",
    PrevUnd0 = false,
    I4 = 0,
    (fun Loop4(I) -> case (I < length(S1)) of true -> Ch = string:substr(S1, (I)+1, ((I + 1))-(I)), (case (((Ch == " ") orelse (Ch == "-")) orelse (Ch == ".")) of true -> (case (not PrevUnd0 andalso (length(Out4) > 0)) of true -> Out5 = Out4 ++ "_", PrevUnd1 = true; _ -> ok end), I5 = (I + 1), throw(continue); _ -> ok end), (case (Ch == "_") of true -> (case (not PrevUnd1 andalso (length(Out5) > 0)) of true -> Out6 = Out5 ++ "_", PrevUnd2 = true; _ -> ok end), I6 = (I + 1), throw(continue); _ -> ok end), (case isUpper(Ch) of undefined -> Out9 = (Out8 + lower(Ch)), PrevUnd4 = false; false -> Out9 = (Out8 + lower(Ch)), PrevUnd4 = false; _ -> (case ((I > 0) andalso (not PrevUnd2)) of true -> Out7 = Out6 ++ "_"; _ -> ok end), Out8 = (Out7 + lower(Ch)), PrevUnd3 = false end), I7 = (I + 1), Loop4(I7); _ -> ok end end(I4)),
    Start2 = 0,
    (fun Loop5(Start) -> case ((Start < length(Out9)) andalso (string:substr(Out9, (Start)+1, ((Start + 1))-(Start)) == "_")) of true -> Start3 = (Start + 1), Loop5(Start3); _ -> ok end end(Start2)),
    End2 = length(Out9),
    (fun Loop6(End) -> case ((End > Start3) andalso (string:substr(Out9, ((End - 1))+1, (End)-((End - 1))) == "_")) of true -> End3 = (End - 1), Loop6(End3); _ -> ok end end(End2)),
    Out10 = string:substr(Out9, (Start3)+1, (End3)-(Start3)),
    Res2 = "",
    J0 = 0,
    LastUnd0 = false,
    (fun Loop7(J) -> case (J < length(Out10)) of true -> C = string:substr(Out10, (J)+1, ((J + 1))-(J)), (case (C == "_") of true -> (case not LastUnd0 of true -> Res3 = (Res2 + C); _ -> ok end), LastUnd1 = true; _ -> Res4 = (Res3 + C), LastUnd2 = false end), J1 = (J + 1), Loop7(J1); _ -> ok end end(J0)),
    Res4.

main() ->
    Samples = ["snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces "],
    io:format("~p~n", ["=== To snake_case ==="]),
    lists:foreach(fun(S) -> io:format("~p~n", [padLeft(S1, 34) ++ " => " ++ camelToSnake(S1)]) end, Samples),
    io:format("~p~n", [""]),
    io:format("~p~n", ["=== To camelCase ==="]),
    lists:foreach(fun(S) -> io:format("~p~n", [padLeft(S1, 34) ++ " => " ++ snakeToCamel(S1)]) end, Samples).

main(_) ->
    main().
