% biorhythms.erl - generated from biorhythms.mochi

sinApprox(X) ->
    Term0 = X,
    Sum0 = X,
    N0 = 1,
    (fun Loop0(Term, Sum, N) -> case (N =< 8) of true -> Denom = ((((2 * N)) * (((2 * N) + 1)))), Term1 = (((-Term * X) * X) / Denom), Sum1 = (Sum + Term), N1 = (N + 1), Loop0(Term1, Sum1, N1); _ -> ok end end(Term0, Sum0, N0)),
    Sum1.

floor(X) ->
    I0 = X,
    (case ((I0) > X) of true -> I1 = (I0 - 1); _ -> ok end),
    I1.

absFloat(X) ->
    case (X < 0) of true -> -X; _ -> X end.

absInt(N) ->
    case (N1 < 0) of true -> -N1; _ -> N1 end.

parseIntStr(Str) ->
    I2 = 0,
    Neg0 = false,
    (case ((length(Str) > 0) andalso (string:substr(Str, (0)+1, (1)-(0)) == "-")) of true -> Neg1 = true, I3 = 1; _ -> ok end),
    N2 = 0,
    Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
    (fun Loop1(N, I) -> case (I < length(Str)) of true -> N3 = ((N * 10) + mochi_get(string:substr(Str, (I)+1, ((I + 1))-(I)), Digits)), I4 = (I + 1), Loop1(I4, N3); _ -> ok end end(N2, I3)),
    (case Neg1 of undefined -> ok; false -> ok; _ -> N4 = -N3 end),
    N4.

parseDate(S) ->
    Y = parseIntStr(string:substr(S, (0)+1, (4)-(0))),
    M = parseIntStr(string:substr(S, (5)+1, (7)-(5))),
    D = parseIntStr(string:substr(S, (8)+1, (10)-(8))),
    [Y, M, D].

leap(Y) ->
    (case (rem(Y, 400) == 0) of true -> true; _ -> ok end),
    (case (rem(Y, 100) == 0) of true -> false; _ -> ok end),
    (rem(Y, 4) == 0).

daysInMonth(Y, M) ->
    Feb = (case leap(Y) of undefined -> 28; false -> 28; _ -> 29 end),
    Lengths = [31, Feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    lists:nth(((M - 1))+1, Lengths).

addDays(Y, M, D, N) ->
    Yy0 = Y,
    Mm0 = M,
    Dd0 = D,
    (case (N4 >= 0) of true -> I5 = 0, (fun Loop2(Dd, I) -> case (I < N4) of true -> Dd1 = (Dd + 1), (case (Dd > daysInMonth(Yy0, Mm0)) of true -> Dd2 = 1, Mm1 = (Mm0 + 1), (case (Mm1 > 12) of true -> Mm2 = 1, Yy1 = (Yy0 + 1); _ -> ok end); _ -> ok end), I6 = (I + 1), Loop2(Dd2, I6); _ -> ok end end(Dd0, I5)); _ -> I7 = 0, (fun Loop3(Dd, I) -> case (I > N4) of true -> Dd3 = (Dd - 1), (case (Dd < 1) of true -> Mm3 = (Mm2 - 1), (case (Mm3 < 1) of true -> Mm4 = 12, Yy2 = (Yy1 - 1); _ -> ok end), Dd4 = daysInMonth(Yy2, Mm4); _ -> ok end), I8 = (I - 1), Loop3(Dd4, I8); _ -> ok end end(Dd2, I7)) end),
    [Yy2, Mm4, Dd4].

pad2(N) ->
    case (N4 < 10) of true -> "0" ++ lists:flatten(io_lib:format("~p", [N4])); _ -> lists:flatten(io_lib:format("~p", [N4])) end.

dateString(Y, M, D) ->
    lists:flatten(io_lib:format("~p", [Y])) ++ "-" ++ pad2(M) ++ "-" ++ pad2(D).

day(Y, M, D) ->
    Part1 = (367 * Y),
    Part2 = ((((7 * (((Y + ((((M + 9)) / 12))))))) / 4)),
    Part3 = ((((275 * M)) / 9)),
    ((((Part1 - Part2) + Part3) + D) - 730530).

biorhythms(Birth, Target) ->
    Bparts = parseDate(Birth),
    By = lists:nth((0)+1, Bparts),
    Bm = lists:nth((1)+1, Bparts),
    Bd = lists:nth((2)+1, Bparts),
    Tparts = parseDate(Target),
    Ty = lists:nth((0)+1, Tparts),
    Tm = lists:nth((1)+1, Tparts),
    Td = lists:nth((2)+1, Tparts),
    Diff = absInt((day(Ty, Tm, Td) - day(By, Bm, Bd))),
    io:format("~p~n", ["Born " ++ Birth ++ ", Target " ++ Target]),
    io:format("~p~n", ["Day " ++ lists:flatten(io_lib:format("~p", [Diff]))]),
    Cycles = ["Physical day ", "Emotional day", "Mental day   "],
    Lengths = [23, 28, 33],
    Quadrants = [["up and rising", "peak"], ["up but falling", "transition"], ["down and falling", "valley"], ["down but rising", "transition"]],
    I9 = 0,
    (fun Loop4(Percent, I) -> case (I < 3) of true -> Length = lists:nth((I)+1, Lengths), Cycle = lists:nth((I)+1, Cycles), Position = rem(Diff, Length), Quadrant = (((Position * 4)) / Length), Percent0 = sinApprox((((2 * 3.141592653589793) * (Position)) / (Length))), Percent1 = (floor((Percent * 1000)) / 10), Description0 = "", (case (Percent > 95) of true -> Description1 = " peak"; _ -> (case (Percent < (-95)) of true -> Description2 = " valley"; _ -> (case (absFloat(Percent) < 5) of true -> Description3 = " critical transition"; _ -> DaysToAdd = (((((Quadrant + 1)) * Length) / 4) - Position), Res = addDays(Ty, Tm, Td, DaysToAdd), Ny = lists:nth((0)+1, Res), Nm = lists:nth((1)+1, Res), Nd = lists:nth((2)+1, Res), Transition = dateString(Ny, Nm, Nd), Trend = lists:nth((0)+1, lists:nth((Quadrant)+1, Quadrants)), Next = lists:nth((1)+1, lists:nth((Quadrant)+1, Quadrants)), Pct0 = lists:flatten(io_lib:format("~p", [Percent])), (case not contains(Pct0, ".") of true -> Pct1 = Pct0 ++ ".0"; _ -> ok end), Description4 = " " ++ Pct1 ++ "% (" ++ Trend ++ ", next " ++ Next ++ " " ++ Transition ++ ")" end) end) end), PosStr0 = lists:flatten(io_lib:format("~p", [Position])), (case (Position < 10) of true -> PosStr1 = " " ++ PosStr0; _ -> ok end), io:format("~p~n", [(Cycle + PosStr1) ++ " : " ++ Description4]), I10 = (I + 1), Loop4(Percent1, I10); _ -> ok end end(Percent, I9)),
    io:format("~p~n", [""]).

main() ->
    Pairs = [["1943-03-09", "1972-07-11"], ["1809-01-12", "1863-11-19"], ["1809-02-12", "1863-11-19"]],
    Idx0 = 0,
    (fun Loop5(Idx) -> case (Idx < length(Pairs)) of true -> P = lists:nth((Idx)+1, Pairs), biorhythms(lists:nth((0)+1, P), lists:nth((1)+1, P)), Idx1 = (Idx + 1), Loop5(Idx1); _ -> ok end end(Idx0)).

main(_) ->
    main().

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
