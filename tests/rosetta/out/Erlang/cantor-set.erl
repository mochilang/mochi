% cantor-set.erl - generated from cantor-set.mochi

setChar(S, Idx, Ch) ->
    ((string:substr(S, (0)+1, (Idx)-(0)) + Ch) + string:substr(S, ((Idx + 1))+1, (length(S))-((Idx + 1)))).

main(_) ->
    Lines0 = [],
    {Lines1} = lists:foldl(fun(I, {Lines}) -> Row0 = "", J0 = 0, (fun Loop0(Row, J) -> case (J < 81) of true -> Row1 = Row ++ "*", J1 = (J + 1), Loop0(Row1, J1); _ -> ok end end(Row0, J0)), Lines1 = Lines ++ [Row1], {Lines1} end, {Lines0}, lists:seq(0, (5)-1)),
    Stack0 = [#{"start" => 0, "len" => 81, "index" => 1}],
    (fun Loop3(Stack) -> case (length(Stack) > 0) of true -> Frame0 = lists:nth(((length(Stack) - 1))+1, Stack), Stack1 = lists:sublist(Stack, (0)+1, ((length(Stack) - 1))-(0)), Start = mochi_get("start", Frame0), LenSeg = mochi_get("len", Frame0), Index = mochi_get("index", Frame0), Seg = ((LenSeg / 3)), (case (Seg == 0) of true -> throw(continue); _ -> ok end), I0 = Index, (fun Loop2(I) -> case (I < 5) of true -> J2 = (Start + Seg), (fun Loop1(Lines, J) -> case (J < (Start + (2 * Seg))) of true -> Lines2 = lists:sublist(Lines, I) ++ [setChar(lists:nth((I)+1, Lines), J, " ")] ++ lists:nthtail((I)+1, Lines), J3 = (J + 1), Loop1(J3, Lines2); _ -> ok end end(Lines1, J2)), I1 = (I + 1), Loop2(I1); _ -> ok end end(I0)), Stack2 = Stack ++ [#{"start" => Start, "len" => Seg, "index" => (Index + 1)}], Stack3 = Stack ++ [#{"start" => (Start + (Seg * 2)), "len" => Seg, "index" => (Index + 1)}], Loop3(Stack3); _ -> ok end end(Stack0)),
    lists:foreach(fun(Line) -> io:format("~p~n", [Line]) end, Lines2).

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
