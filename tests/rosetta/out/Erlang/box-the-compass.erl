% box-the-compass.erl - generated from box-the-compass.mochi

padLeft(S, W) ->
    Res0 = "",
    N0 = (W - length(S)),
    (fun Loop0(Res, N) -> case (N > 0) of true -> Res1 = Res ++ " ", N1 = (N - 1), Loop0(Res1, N1); _ -> ok end end(Res0, N0)),
    (Res1 + S).

padRight(S, W) ->
    Out0 = S,
    I0 = length(S),
    (fun Loop1(Out, I) -> case (I < W) of true -> Out1 = Out ++ " ", I1 = (I + 1), Loop1(Out1, I1); _ -> ok end end(Out0, I0)),
    Out1.

indexOf(S, Ch) ->
    I2 = 0,
    (fun Loop2(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I3 = (I + 1), Loop2(I3); _ -> ok end end(I2)),
    -1.

format2(F) ->
    S0 = lists:flatten(io_lib:format("~p", [F])),
    Idx = indexOf(S0, "."),
    (case (Idx < 0) of true -> S1 = S0 ++ ".00"; _ -> Need0 = (Idx + 3), (case (length(S1) > Need0) of true -> S2 = string:substr(S1, (0)+1, (Need0)-(0)); _ -> (fun Loop3(S) -> case (length(S) < Need0) of true -> S3 = S ++ "0", Loop3(S3); _ -> ok end end(S2)) end) end),
    S3.

cpx(H) ->
    X0 = ((((H / 11.25)) + 0.5)),
    X1 = rem(X0, 32),
    (case (X1 < 0) of true -> X2 = (X1 + 32); _ -> ok end),
    X2.

degrees2compasspoint(H) ->
    mochi_get(cpx(H), CompassPoint).

main(_) ->
    CompassPoint = ["North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west"],
    Headings = [0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38],
    io:format("~p~n", ["Index  Compass point         Degree"]),
    I4 = 0,
    (fun Loop4(I) -> case (I < length(Headings)) of true -> H = lists:nth((I)+1, Headings), Idx = (rem(I, 32) + 1), Cp = degrees2compasspoint(H), io:format("~p~n", [padLeft(lists:flatten(io_lib:format("~p", [Idx])), 4) ++ "   " ++ padRight(Cp, 19) ++ " " ++ format2(H) ++ "°"]), I5 = (I + 1), Loop4(I5); _ -> ok end end(I4)).

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
