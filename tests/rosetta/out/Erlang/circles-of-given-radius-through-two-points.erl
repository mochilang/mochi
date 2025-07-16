% circles-of-given-radius-through-two-points.erl - generated from circles-of-given-radius-through-two-points.mochi

sqrtApprox(X) ->
    G0 = X,
    I0 = 0,
    (fun Loop0(G, I) -> case (I < 40) of true -> G1 = (((G + (X / G))) / 2), I1 = (I + 1), Loop0(G1, I1); _ -> ok end end(G0, I0)),
    G1.

hypot(X, Y) ->
    sqrtApprox(((X * X) + (Y * Y))).

circles(P1, P2, R) ->
    (case ((mochi_get(x, P1) == mochi_get(x, P2)) andalso (mochi_get(y, P1) == mochi_get(y, P2))) of true -> (case (R == 0) of true -> [P1, P1, "Coincident points with r==0.0 describe a degenerate circle."]; _ -> ok end), [P1, P2, "Coincident points describe an infinite number of circles."]; _ -> ok end),
    (case (R == 0) of true -> [P1, P2, "R==0.0 does not describe circles."]; _ -> ok end),
    Dx = (mochi_get(x, P2) - mochi_get(x, P1)),
    Dy = (mochi_get(y, P2) - mochi_get(y, P1)),
    Q = hypot(Dx, Dy),
    (case (Q > (2 * R)) of true -> [P1, P2, "Points too far apart to form circles."]; _ -> ok end),
    M = #{"__name" => "Point", x => (((mochi_get(x, P1) + mochi_get(x, P2))) / 2), y => (((mochi_get(y, P1) + mochi_get(y, P2))) / 2)},
    (case (Q == (2 * R)) of true -> [M, M, "Points form a diameter and describe only a single circle."]; _ -> ok end),
    D = sqrtApprox(((R * R) - ((Q * Q) / 4))),
    Ox = ((D * Dx) / Q),
    Oy = ((D * Dy) / Q),
    [#{"__name" => "Point", x => (mochi_get(x, M) - Oy), y => (mochi_get(y, M) + Ox)}, #{"__name" => "Point", x => (mochi_get(x, M) + Oy), y => (mochi_get(y, M) - Ox)}, "Two circles."].

main(_) ->
    Td0 = [[#{"__name" => "Point", x => 0.1234, y => 0.9876}, #{"__name" => "Point", x => 0.8765, y => 0.2345}, 2], [#{"__name" => "Point", x => 0, y => 2}, #{"__name" => "Point", x => 0, y => 0}, 1], [#{"__name" => "Point", x => 0.1234, y => 0.9876}, #{"__name" => "Point", x => 0.1234, y => 0.9876}, 2], [#{"__name" => "Point", x => 0.1234, y => 0.9876}, #{"__name" => "Point", x => 0.8765, y => 0.2345}, 0.5], [#{"__name" => "Point", x => 0.1234, y => 0.9876}, #{"__name" => "Point", x => 0.1234, y => 0.9876}, 0]],
    lists:foreach(fun(Tc) -> P1 = lists:nth((0)+1, Tc), P2 = lists:nth((1)+1, Tc), R = lists:nth((2)+1, Tc), io:format("~p~n", ["p1:  {" ++ lists:flatten(io_lib:format("~p", [mochi_get(x, P1)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(y, P1)])) ++ "}"]), io:format("~p~n", ["p2:  {" ++ lists:flatten(io_lib:format("~p", [mochi_get(x, P2)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(y, P2)])) ++ "}"]), io:format("~p~n", ["r:  " ++ lists:flatten(io_lib:format("~p", [R]))]), Res = circles(P1, P2, R), C1 = lists:nth((0)+1, Res), C2 = lists:nth((1)+1, Res), CaseStr = lists:nth((2)+1, Res), io:format("~p~n", ["   " ++ CaseStr]), (case ((CaseStr == "Points form a diameter and describe only a single circle.") orelse (CaseStr == "Coincident points with r==0.0 describe a degenerate circle.")) of true -> io:format("~p~n", ["   Center:  {" ++ lists:flatten(io_lib:format("~p", [mochi_get(x, C1)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(y, C1)])) ++ "}"]); _ -> (case (CaseStr == "Two circles.") of true -> io:format("~p~n", ["   Center 1:  {" ++ lists:flatten(io_lib:format("~p", [mochi_get(x, C1)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(y, C1)])) ++ "}"]), io:format("~p~n", ["   Center 2:  {" ++ lists:flatten(io_lib:format("~p", [mochi_get(x, C2)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(y, C2)])) ++ "}"]); _ -> ok end) end), io:format("~p~n", [""]) end, Td0).

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
