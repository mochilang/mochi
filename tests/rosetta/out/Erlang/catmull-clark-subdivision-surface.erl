% catmull-clark-subdivision-surface.erl - generated from catmull-clark-subdivision-surface.mochi

indexOf(S, Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(S)) of true -> (case (string:substr(S, (I)+1, ((I + 1))-(I)) == Ch) of true -> I; _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    -1.

fmt4(X) ->
    Y0 = (X * 10000),
    (case (Y0 >= 0) of true -> Y1 = (Y0 + 0.5); _ -> Y2 = (Y1 - 0.5) end),
    Y3 = ((Y2) / 10000),
    S0 = lists:flatten(io_lib:format("~p", [Y3])),
    Dot0 = indexOf(S0, "."),
    (case (Dot0 == (0 - 1)) of true -> S1 = S0 ++ ".0000"; _ -> Decs0 = ((length(S1) - Dot0) - 1), (case (Decs0 > 4) of true -> S2 = string:substr(S1, (0)+1, ((Dot0 + 5))-(0)); _ -> (fun Loop1(S, Decs) -> case (Decs < 4) of true -> S3 = S ++ "0", Decs1 = (Decs + 1), Loop1(S3, Decs1); _ -> ok end end(S2, Decs0)) end) end),
    (case (X >= 0) of true -> S4 = " " ++ S3; _ -> ok end),
    S4.

fmt2(N) ->
    S = lists:flatten(io_lib:format("~p", [N])),
    (case (length(S) < 2) of true -> " " ++ S; _ -> ok end),
    S.

sumPoint(P1, P2) ->
    #{"__name" => "Point", x => (mochi_get(x, P1) + mochi_get(x, P2)), y => (mochi_get(y, P1) + mochi_get(y, P2)), z => (mochi_get(z, P1) + mochi_get(z, P2))}.

mulPoint(P, M) ->
    #{"__name" => "Point", x => (mochi_get(x, P) * M), y => (mochi_get(y, P) * M), z => (mochi_get(z, P) * M)}.

divPoint(P, D) ->
    mulPoint(P, (1 / D)).

centerPoint(P1, P2) ->
    divPoint(sumPoint(P1, P2), 2).

getFacePoints(Points, Faces) ->
    FacePoints0 = [],
    I2 = 0,
    (fun Loop2(FacePoints, I, Fp) -> case (I < length(Faces)) of true -> Face = lists:nth((I)+1, Faces), Fp0 = #{"__name" => "Point", x => 0, y => 0, z => 0}, {Fp1} = lists:foldl(fun(Idx, {Fp}) -> Fp1 = sumPoint(Fp, lists:nth((Idx)+1, Points)), {Fp1} end, {Fp}, Face), Fp2 = divPoint(Fp1, (length(Face))), FacePoints1 = FacePoints ++ [Fp2], I3 = (I + 1), Loop2(Fp2, FacePoints1, I3); _ -> ok end end(FacePoints0, I2, Fp)),
    FacePoints1.

sortEdges(Edges) ->
    Res0 = [],
    Tmp0 = Edges,
    (fun Loop5(Res, Tmp) -> case (length(Tmp) > 0) of true -> Min0 = lists:nth((0)+1, Tmp), Idx0 = 0, J0 = 1, (fun Loop3(J) -> case (J < length(Tmp)) of true -> E = mochi_get(J, Tmp), (case ((lists:nth((0)+1, E) < lists:nth((0)+1, Min0)) orelse (((lists:nth((0)+1, E) == lists:nth((0)+1, Min0)) andalso (((lists:nth((1)+1, E) < lists:nth((1)+1, Min0)) orelse (((lists:nth((1)+1, E) == lists:nth((1)+1, Min0)) andalso (lists:nth((2)+1, E) < lists:nth((2)+1, Min0))))))))) of true -> Min1 = E, Idx1 = J; _ -> ok end), J1 = (J + 1), Loop3(J1); _ -> ok end end(J0)), Res1 = Res ++ [Min1], Out0 = [], K0 = 0, (fun Loop4(K) -> case (K < length(Tmp)) of true -> (case (K /= Idx1) of true -> Out1 = Out0 ++ [mochi_get(K, Tmp)]; _ -> ok end), K1 = (K + 1), Loop4(K1); _ -> ok end end(K0)), Tmp1 = Out1, Loop5(Res1, Tmp1); _ -> ok end end(Res0, Tmp0)),
    Res1.

getEdgesFaces(Points, Faces) ->
    Edges0 = [],
    Fnum0 = 0,
    (fun Loop7(Fnum) -> case (Fnum < length(Faces)) of true -> Face = lists:nth((Fnum)+1, Faces), NumP0 = length(Face), Pi0 = 0, (fun Loop6(Edges, Pi) -> case (Pi < NumP0) of true -> Pn10 = mochi_get(Pi, Face), Pn20 = 0, (case (Pi < (NumP0 - 1)) of true -> Pn21 = mochi_get((Pi + 1), Face); _ -> Pn22 = lists:nth((0)+1, Face) end), (case (Pn10 > Pn22) of true -> Tmpn0 = Pn10, Pn11 = Pn22, Pn23 = Tmpn0; _ -> ok end), Edges1 = Edges ++ [[Pn11, Pn23, Fnum]], Pi1 = (Pi + 1), Loop6(Edges1, Pi1); _ -> ok end end(Edges0, Pi0)), Fnum1 = (Fnum + 1), Loop7(Fnum1); _ -> ok end end(Fnum0)),
    Edges2 = sortEdges(Edges1),
    Merged0 = [],
    Idx2 = 0,
    (fun Loop8(Merged, Idx) -> case (Idx < length(Edges2)) of true -> E1 = lists:nth((Idx)+1, Edges2), (case (Idx < (length(Edges2) - 1)) of true -> E2 = lists:nth(((Idx + 1))+1, Edges2), (case ((lists:nth((0)+1, E1) == lists:nth((0)+1, E2)) andalso (lists:nth((1)+1, E1) == lists:nth((1)+1, E2))) of true -> Merged1 = Merged ++ [[lists:nth((0)+1, E1), lists:nth((1)+1, E1), lists:nth((2)+1, E1), lists:nth((2)+1, E2)]], Idx3 = (Idx + 2), throw(continue); _ -> ok end); _ -> ok end), Merged2 = Merged ++ [[lists:nth((0)+1, E1), lists:nth((1)+1, E1), lists:nth((2)+1, E1), -1]], Idx4 = (Idx + 1), Loop8(Merged2, Idx4); _ -> ok end end(Merged0, Idx2)),
    EdgesCenters0 = [],
    {EdgesCenters1} = lists:foldl(fun(Me, {EdgesCenters}) -> P1 = lists:nth((lists:nth((0)+1, Me))+1, Points), P2 = lists:nth((lists:nth((1)+1, Me))+1, Points), Cp = centerPoint(P1, P2), EdgesCenters1 = EdgesCenters ++ [#{"__name" => "Edge", pn1 => lists:nth((0)+1, Me), pn2 => lists:nth((1)+1, Me), fn1 => lists:nth((2)+1, Me), fn2 => lists:nth((3)+1, Me), cp => Cp}], {EdgesCenters1} end, {EdgesCenters0}, Merged2),
    EdgesCenters1.

getEdgePoints(Points, EdgesFaces, FacePoints) ->
    EdgePoints0 = [],
    I4 = 0,
    (fun Loop9(EdgePoints, I) -> case (I < length(EdgesFaces)) of true -> Edge = lists:nth((I)+1, EdgesFaces), Cp = mochi_get(cp, Edge), Fp1 = lists:nth((mochi_get(fn1, Edge))+1, FacePoints1), Fp20 = Fp1, (case (mochi_get(fn2, Edge) /= (0 - 1)) of true -> Fp21 = lists:nth((mochi_get(fn2, Edge))+1, FacePoints1); _ -> ok end), Cfp = centerPoint(Fp1, Fp21), EdgePoints1 = EdgePoints ++ [centerPoint(Cp, Cfp)], I5 = (I + 1), Loop9(EdgePoints1, I5); _ -> ok end end(EdgePoints0, I4)),
    EdgePoints1.

getAvgFacePoints(Points, Faces, FacePoints) ->
    NumP1 = length(Points),
    Temp0 = [],
    I6 = 0,
    (fun Loop10(Temp, I) -> case (I < NumP1) of true -> Temp1 = Temp ++ [#{"__name" => "PointEx", p => #{"__name" => "Point", x => 0, y => 0, z => 0}, n => 0}], I7 = (I + 1), Loop10(Temp1, I7); _ -> ok end end(Temp0, I6)),
    Fnum2 = 0,
    (fun Loop11(Fnum) -> case (Fnum < length(Faces)) of true -> Fp = lists:nth((Fnum)+1, FacePoints1), {Temp2} = lists:foldl(fun(Pn, {Temp}) -> Tp = lists:nth((Pn)+1, Temp), Temp2 = lists:sublist(Temp, Pn) ++ [#{"__name" => "PointEx", p => sumPoint(mochi_get(p, Tp), Fp), n => (mochi_get(n, Tp) + 1)}] ++ lists:nthtail((Pn)+1, Temp), {Temp2} end, {Temp1}, lists:nth((Fnum)+1, Faces)), Fnum3 = (Fnum + 1), Loop11(Fnum3); _ -> ok end end(Fnum2)),
    Avg0 = [],
    J2 = 0,
    (fun Loop12(Avg, J) -> case (J < NumP1) of true -> Tp = lists:nth((J)+1, Temp2), Avg1 = Avg ++ [divPoint(mochi_get(p, Tp), mochi_get(n, Tp))], J3 = (J + 1), Loop12(Avg1, J3); _ -> ok end end(Avg0, J2)),
    Avg1.

getAvgMidEdges(Points, EdgesFaces) ->
    NumP2 = length(Points),
    Temp3 = [],
    I8 = 0,
    (fun Loop13(Temp, I) -> case (I < NumP2) of true -> Temp4 = Temp ++ [#{"__name" => "PointEx", p => #{"__name" => "Point", x => 0, y => 0, z => 0}, n => 0}], I9 = (I + 1), Loop13(Temp4, I9); _ -> ok end end(Temp3, I8)),
    lists:foreach(fun(Edge) -> Cp = mochi_get(cp, Edge), Arr0 = [mochi_get(pn1, Edge), mochi_get(pn2, Edge)], {Temp5} = lists:foldl(fun(Pn, {Temp}) -> Tp = lists:nth((Pn)+1, Temp), Temp5 = lists:sublist(Temp, Pn) ++ [#{"__name" => "PointEx", p => sumPoint(mochi_get(p, Tp), Cp), n => (mochi_get(n, Tp) + 1)}] ++ lists:nthtail((Pn)+1, Temp), {Temp5} end, {Temp4}, Arr0) end, EdgesFaces),
    Avg2 = [],
    J4 = 0,
    (fun Loop14(Avg, J) -> case (J < NumP2) of true -> Tp = lists:nth((J)+1, Temp5), Avg3 = Avg ++ [divPoint(mochi_get(p, Tp), mochi_get(n, Tp))], J5 = (J + 1), Loop14(Avg3, J5); _ -> ok end end(Avg2, J4)),
    Avg3.

getPointsFaces(Points, Faces) ->
    Pf0 = [],
    I10 = 0,
    (fun Loop15(Pf, I) -> case (I < length(Points)) of true -> Pf1 = Pf ++ [0], I11 = (I + 1), Loop15(Pf1, I11); _ -> ok end end(Pf0, I10)),
    Fnum4 = 0,
    (fun Loop16(Fnum) -> case (Fnum < length(Faces)) of true -> {Pf2} = lists:foldl(fun(Pn, {Pf}) -> Pf2 = lists:sublist(Pf, Pn) ++ [(lists:nth((Pn)+1, Pf) + 1)] ++ lists:nthtail((Pn)+1, Pf), {Pf2} end, {Pf1}, lists:nth((Fnum)+1, Faces)), Fnum5 = (Fnum + 1), Loop16(Fnum5); _ -> ok end end(Fnum4)),
    Pf2.

getNewPoints(Points, Pf, Afp, Ame) ->
    NewPts0 = [],
    I12 = 0,
    (fun Loop17(NewPts, I) -> case (I < length(Points)) of true -> N0 = lists:nth((I)+1, Pf2), M10 = (((N0 - 3)) / N0), M20 = (1 / N0), M30 = (2 / N0), Old = lists:nth((I)+1, Points), P1 = mulPoint(Old, M10), P2 = mulPoint(lists:nth((I)+1, Afp), M20), P3 = mulPoint(lists:nth((I)+1, Ame), M30), NewPts1 = NewPts ++ [sumPoint(sumPoint(P1, P2), P3)], I13 = (I + 1), Loop17(NewPts1, I13); _ -> ok end end(NewPts0, I12)),
    NewPts1.

key(A, B) ->
    case (A < B) of true -> lists:flatten(io_lib:format("~p", [A])) ++ "," ++ lists:flatten(io_lib:format("~p", [B])); _ -> lists:flatten(io_lib:format("~p", [B])) ++ "," ++ lists:flatten(io_lib:format("~p", [A])) end.

cmcSubdiv(Points, Faces) ->
    FacePoints = getFacePoints(Points, Faces),
    EdgesFaces = getEdgesFaces(Points, Faces),
    EdgePoints = getEdgePoints(Points, EdgesFaces, FacePoints),
    AvgFacePoints = getAvgFacePoints(Points, Faces, FacePoints),
    AvgMidEdges = getAvgMidEdges(Points, EdgesFaces),
    PointsFaces = getPointsFaces(Points, Faces),
    NewPoints0 = getNewPoints(Points, PointsFaces, AvgFacePoints, AvgMidEdges),
    FacePointNums0 = [],
    NextPoint0 = length(NewPoints0),
    {FacePointNums1, NextPoint1, NewPoints1} = lists:foldl(fun(Fp, {NewPoints, FacePointNums, NextPoint}) -> NewPoints1 = NewPoints ++ [Fp], FacePointNums1 = FacePointNums ++ [NextPoint], NextPoint1 = (NextPoint + 1), {FacePointNums1, NextPoint1, NewPoints1} end, {NewPoints0, FacePointNums0, NextPoint0}, FacePoints),
    EdgePointNums0 = #{},
    Idx5 = 0,
    (fun Loop18(NewPoints, EdgePointNums, NextPoint, Idx) -> case (Idx < length(EdgesFaces)) of true -> E = lists:nth((Idx)+1, EdgesFaces), NewPoints2 = NewPoints ++ [lists:nth((Idx)+1, EdgePoints)], EdgePointNums1 = maps:put(key(mochi_get(pn1, E), mochi_get(pn2, E)), NextPoint, EdgePointNums), NextPoint2 = (NextPoint + 1), Idx6 = (Idx + 1), Loop18(NextPoint2, Idx6, NewPoints2, EdgePointNums1); _ -> ok end end(NewPoints1, EdgePointNums0, NextPoint1, Idx5)),
    NewFaces0 = [],
    Fnum6 = 0,
    (fun Loop19(Fnum) -> case (Fnum < length(Faces)) of true -> OldFace = lists:nth((Fnum)+1, Faces), (case (length(OldFace) == 4) of true -> A = lists:nth((0)+1, OldFace), B = lists:nth((1)+1, OldFace), C = lists:nth((2)+1, OldFace), D = lists:nth((3)+1, OldFace), Fpnum = lists:nth((Fnum)+1, FacePointNums1), Ab = mochi_get(key(A, B), EdgePointNums1), Da = mochi_get(key(D, A), EdgePointNums1), Bc = mochi_get(key(B, C), EdgePointNums1), Cd = mochi_get(key(C, D), EdgePointNums1), NewFaces1 = NewFaces0 ++ [[A, Ab, Fpnum, Da]], NewFaces2 = NewFaces1 ++ [[B, Bc, Fpnum, Ab]], NewFaces3 = NewFaces2 ++ [[C, Cd, Fpnum, Bc]], NewFaces4 = NewFaces3 ++ [[D, Da, Fpnum, Cd]]; _ -> ok end), Fnum7 = (Fnum + 1), Loop19(Fnum7); _ -> ok end end(Fnum6)),
    [NewPoints2, NewFaces4].

formatPoint(P) ->
    "[" ++ fmt4(mochi_get(x, P)) ++ " " ++ fmt4(mochi_get(y, P)) ++ " " ++ fmt4(mochi_get(z, P)) ++ "]".

formatFace(F) ->
    (case (length(F) == 0) of true -> "[]"; _ -> ok end),
    S5 = "[" ++ fmt2(lists:nth((0)+1, F)),
    I14 = 1,
    (fun Loop20(S, I) -> case (I < length(F)) of true -> S6 = S ++ " " ++ fmt2(lists:nth((I)+1, F)), I15 = (I + 1), Loop20(S6, I15); _ -> ok end end(S, I14)),
    S7 = S6 ++ "]",
    S7.

main() ->
    InputPoints = [#{"__name" => "Point", x => -1, y => 1, z => 1}, #{"__name" => "Point", x => -1, y => -1, z => 1}, #{"__name" => "Point", x => 1, y => -1, z => 1}, #{"__name" => "Point", x => 1, y => 1, z => 1}, #{"__name" => "Point", x => 1, y => -1, z => -1}, #{"__name" => "Point", x => 1, y => 1, z => -1}, #{"__name" => "Point", x => -1, y => -1, z => -1}, #{"__name" => "Point", x => -1, y => 1, z => -1}],
    InputFaces = [[0, 1, 2, 3], [3, 2, 4, 5], [5, 4, 6, 7], [7, 0, 3, 5], [7, 6, 1, 0], [6, 1, 2, 4]],
    OutputPoints0 = InputPoints,
    OutputFaces0 = InputFaces,
    I16 = 0,
    (fun Loop21(OutputPoints, OutputFaces, I) -> case (I < 1) of true -> Res = cmcSubdiv(OutputPoints, OutputFaces), OutputPoints1 = lists:nth((0)+1, Res), OutputFaces1 = lists:nth((1)+1, Res), I17 = (I + 1), Loop21(OutputPoints1, OutputFaces1, I17); _ -> ok end end(OutputPoints0, OutputFaces0, I16)),
    lists:foreach(fun(P) -> io:format("~p~n", [formatPoint(P)]) end, OutputPoints1),
    io:format("~p~n", [""]),
    lists:foreach(fun(F) -> io:format("~p~n", [formatFace(F)]) end, OutputFaces1).

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
