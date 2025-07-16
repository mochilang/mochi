% align-columns.erl - generated from align-columns.mochi

split(S, Sep) ->
    Parts0 = [],
    Cur0 = "",
    I0 = 0,
    (fun Loop0() -> case (I0 < length(S)) of true -> (case (((length(Sep) > 0) andalso ((I0 + length(Sep)) =< length(S))) andalso (string:substr(S, (I0)+1, ((I0 + length(Sep)))-(I0)) == Sep)) of true -> Parts1 = Parts0 ++ [Cur0], Cur1 = "", I1 = (I0 + length(Sep)); _ -> Cur2 = (Cur1 + string:substr(S, (I1)+1, ((I1 + 1))-(I1))), I2 = (I1 + 1) end), Loop0(); _ -> ok end end()),
    Parts2 = Parts1 ++ [Cur2],
    Parts2.

rstripEmpty(Words) ->
    N0 = length(Words),
    (fun Loop1(N) -> case ((N > 0) andalso (lists:nth(((N - 1))+1, Words) == "")) of true -> N1 = (N - 1), Loop1(N1); _ -> ok end end(N0)),
    lists:sublist(Words, (0)+1, (N1)-(0)).

spaces(N) ->
    Out0 = "",
    I3 = 0,
    (fun Loop2(Out, I) -> case (I < N1) of true -> Out1 = Out ++ " ", I4 = (I + 1), Loop2(Out1, I4); _ -> ok end end(Out0, I3)),
    Out1.

pad(Word, Width, Align) ->
    Diff = (Width - length(Word)),
    (case (Align == 0) of true -> (Word + spaces(Diff)); _ -> ok end),
    (case (Align == 2) of true -> (spaces(Diff) + Word); _ -> ok end),
    Left0 = ((Diff / 2)),
    Right0 = (Diff - Left0),
    ((spaces(Left0) + Word) + spaces(Right0)).

newFormatter(Text) ->
    Lines0 = split(Text, "\n"),
    FmtLines0 = [],
    Width0 = [],
    I5 = 0,
    (fun Loop4(FmtLines, I) -> case (I < length(Lines0)) of true -> (case (length(mochi_get(I, Lines0)) == 0) of true -> I6 = (I + 1), throw(continue); _ -> ok end), Words0 = rstripEmpty(split(mochi_get(I, Lines0), "$")), FmtLines1 = FmtLines ++ [Words0], J0 = 0, (fun Loop3(J) -> case (J < length(Words0)) of true -> Wlen = length(lists:nth((J)+1, Words0)), (case (J == length(Width0)) of true -> Width1 = Width0 ++ [Wlen]; _ -> (case (Wlen > lists:nth((J)+1, Width1)) of true -> Width2 = lists:sublist(Width1, J) ++ [Wlen] ++ lists:nthtail((J)+1, Width1); _ -> ok end) end), J1 = (J + 1), Loop3(J1); _ -> ok end end(J0)), I7 = (I + 1), Loop4(FmtLines1, I7); _ -> ok end end(FmtLines0, I5)),
    #{"text" => FmtLines1, "width" => Width2}.

printFmt(F, Align) ->
    Lines = mochi_get("text", F),
    Width = mochi_get("width", F),
    I8 = 0,
    (fun Loop6(I) -> case (I < length(Lines)) of true -> Words = mochi_get(I, Lines), Line0 = "", J2 = 0, (fun Loop5(Line, J) -> case (J < length(Words)) of true -> Line1 = (Line + pad(lists:nth((J)+1, Words), lists:nth((J)+1, Width), Align)) ++ " ", J3 = (J + 1), Loop5(Line1, J3); _ -> ok end end(Line0, J2)), io:format("~p~n", [Line1]), I9 = (I + 1), Loop6(I9); _ -> ok end end(I8)),
    io:format("~p~n", [""]).

main(_) ->
    Text = "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" ++ "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" ++ "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n" ++ "column$are$separated$by$at$least$one$space.\n" ++ "Further,$allow$for$each$word$in$a$column$to$be$either$left\n" ++ "justified,$right$justified,$or$center$justified$within$its$column.",
    F = newFormatter(Text),
    printFmt(F, 0),
    printFmt(F, 1),
    printFmt(F, 2).

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
