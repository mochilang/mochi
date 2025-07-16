% avl-tree.erl - generated from avl-tree.mochi

Node(Data) ->
    #{"Data" => Data, "Balance" => 0, "Link" => [undefined, undefined]}.

getLink(N, Dir) ->
    mochi_get(Dir, (mochi_get("Link", N))).

setLink(N, Dir, V) ->
    Links0 = mochi_get("Link", N),
    Links1 = maps:put(Dir, V, Links0),
    N0 = maps:put("Link", Links1, N).

opp(Dir) ->
    (1 - Dir).

single(Root, Dir) ->
    Tmp0 = getLink(Root, opp(Dir)),
    setLink(Root, opp(Dir), getLink(Tmp0, Dir)),
    setLink(Tmp0, Dir, Root),
    Tmp0.

double(Root, Dir) ->
    Tmp1 = getLink(getLink(Root, opp(Dir)), Dir),
    setLink(getLink(Root, opp(Dir)), Dir, getLink(Tmp1, opp(Dir))),
    setLink(Tmp1, opp(Dir), getLink(Root, opp(Dir))),
    setLink(Root, opp(Dir), Tmp1),
    Tmp2 = getLink(Root, opp(Dir)),
    setLink(Root, opp(Dir), getLink(Tmp2, Dir)),
    setLink(Tmp2, Dir, Root),
    Tmp2.

adjustBalance(Root, Dir, Bal) ->
    N1 = getLink(Root, Dir),
    Nn0 = getLink(N1, opp(Dir)),
    (case (mochi_get("Balance", Nn0) == 0) of true -> Root0 = maps:put("Balance", 0, Root), N2 = maps:put("Balance", 0, N1); _ -> (case (mochi_get("Balance", Nn0) == Bal) of true -> Root1 = maps:put("Balance", -Bal, Root0), N3 = maps:put("Balance", 0, N2); _ -> Root2 = maps:put("Balance", 0, Root1), N4 = maps:put("Balance", Bal, N3) end) end),
    Nn1 = maps:put("Balance", 0, Nn0).

insertBalance(Root, Dir) ->
    N5 = getLink(Root2, Dir),
    Bal0 = ((2 * Dir) - 1),
    (case (mochi_get("Balance", N5) == Bal0) of true -> Root3 = maps:put("Balance", 0, Root2), N6 = maps:put("Balance", 0, N5), single(Root3, opp(Dir)); _ -> ok end),
    adjustBalance(Root3, Dir, Bal0),
    double(Root3, opp(Dir)).

insertR(Root, Data) ->
    (case (Root3 == undefined) of true -> #{"node" => Node(Data), "done" => false}; _ -> ok end),
    Node0 = Root3,
    Dir0 = 0,
    (case ((mochi_get("Data", Node0)) < Data) of true -> Dir1 = 1; _ -> ok end),
    R0 = insertR(getLink(Node0, Dir1), Data),
    setLink(Node0, Dir1, mochi_get("node", R0)),
    (case mochi_get("done", R0) of undefined -> ok; false -> ok; _ -> #{"node" => Node0, "done" => true} end),
    Node1 = maps:put("Balance", ((mochi_get("Balance", Node0)) + (((2 * Dir1) - 1))), Node0),
    (case (mochi_get("Balance", Node1) == 0) of true -> #{"node" => Node1, "done" => true}; _ -> ok end),
    (case ((mochi_get("Balance", Node1) == 1) orelse (mochi_get("Balance", Node1) == (-1))) of true -> #{"node" => Node1, "done" => false}; _ -> ok end),
    #{"node" => insertBalance(Node1, Dir1), "done" => true}.

Insert(Tree, Data) ->
    R = insertR(Tree, Data),
    mochi_get("node", R).

removeBalance(Root, Dir) ->
    N7 = getLink(Root3, opp(Dir1)),
    Bal1 = ((2 * Dir1) - 1),
    (case (mochi_get("Balance", N7) == (-Bal1)) of true -> Root4 = maps:put("Balance", 0, Root3), N8 = maps:put("Balance", 0, N7), #{"node" => single(Root4, Dir1), "done" => false}; _ -> ok end),
    (case (mochi_get("Balance", N8) == Bal1) of true -> adjustBalance(Root4, opp(Dir1), (-Bal1)), #{"node" => double(Root4, Dir1), "done" => false}; _ -> ok end),
    Root5 = maps:put("Balance", -Bal1, Root4),
    N9 = maps:put("Balance", Bal1, N8),
    #{"node" => single(Root5, Dir1), "done" => true}.

removeR(Root, Data) ->
    (case (Root5 == undefined) of true -> #{"node" => undefined, "done" => false}; _ -> ok end),
    Node2 = Root5,
    (case ((mochi_get("Data", Node2)) == Data) of true -> (case (getLink(Node2, 0) == undefined) of true -> #{"node" => getLink(Node2, 1), "done" => false}; _ -> ok end), (case (getLink(Node2, 1) == undefined) of true -> #{"node" => getLink(Node2, 0), "done" => false}; _ -> ok end), Heir0 = getLink(Node2, 0), (fun Loop0(Heir) -> case (getLink(Heir, 1) /= undefined) of true -> Heir1 = getLink(Heir, 1), Loop0(Heir1); _ -> ok end end(Heir0)), Node3 = maps:put("Data", mochi_get("Data", Heir1), Node2), Data0 = mochi_get("Data", Heir1); _ -> ok end),
    Dir2 = 0,
    (case ((mochi_get("Data", Node3)) < Data0) of true -> Dir3 = 1; _ -> ok end),
    R1 = removeR(getLink(Node3, Dir3), Data0),
    setLink(Node3, Dir3, mochi_get("node", R)),
    (case mochi_get("done", R) of undefined -> ok; false -> ok; _ -> #{"node" => Node3, "done" => true} end),
    Node4 = maps:put("Balance", (((mochi_get("Balance", Node3)) + 1) - (2 * Dir3)), Node3),
    (case ((mochi_get("Balance", Node4) == 1) orelse (mochi_get("Balance", Node4) == (-1))) of true -> #{"node" => Node4, "done" => true}; _ -> ok end),
    (case (mochi_get("Balance", Node4) == 0) of true -> #{"node" => Node4, "done" => false}; _ -> ok end),
    removeBalance(Node4, Dir3).

Remove(Tree, Data) ->
    R = removeR(Tree, Data0),
    mochi_get("node", R).

indentStr(N) ->
    S0 = "",
    I0 = 0,
    (fun Loop1(I, S) -> case (I < N9) of true -> S1 = S ++ " ", I1 = (I + 1), Loop1(S1, I1); _ -> ok end end(I0, S0)),
    S1.

dumpNode(Node, Indent, Comma) ->
    Sp = indentStr(Indent),
    (case (Node4 == undefined) of true -> Line0 = Sp ++ "null", (case Comma of undefined -> ok; false -> ok; _ -> Line1 = Line0 ++ "," end), io:format("~p~n", [Line1]); _ -> io:format("~p~n", [Sp ++ "{"]), io:format("~p~n", [indentStr((Indent + 3)) ++ "\"Data\": " ++ lists:flatten(io_lib:format("~p", [mochi_get("Data", Node4)])) ++ ","]), io:format("~p~n", [indentStr((Indent + 3)) ++ "\"Balance\": " ++ lists:flatten(io_lib:format("~p", [mochi_get("Balance", Node4)])) ++ ","]), io:format("~p~n", [indentStr((Indent + 3)) ++ "\"Link\": ["]), dumpNode(getLink(Node4, 0), (Indent + 6), true), dumpNode(getLink(Node4, 1), (Indent + 6), false), io:format("~p~n", [indentStr((Indent + 3)) ++ "]"]), End0 = Sp ++ "}", (case Comma of undefined -> ok; false -> ok; _ -> End1 = End0 ++ "," end), io:format("~p~n", [End1]) end).

dump(Node, Indent) ->
    dumpNode(Node4, Indent, false).

main() ->
    Tree0 = undefined,
    io:format("~p~n", ["Empty tree:"]),
    dump(Tree0, 0),
    io:format("~p~n", [""]),
    io:format("~p~n", ["Insert test:"]),
    Tree1 = Insert(Tree0, 3),
    Tree2 = Insert(Tree1, 1),
    Tree3 = Insert(Tree2, 4),
    Tree4 = Insert(Tree3, 1),
    Tree5 = Insert(Tree4, 5),
    dump(Tree5, 0),
    io:format("~p~n", [""]),
    io:format("~p~n", ["Remove test:"]),
    Tree6 = Remove(Tree5, 3),
    Tree7 = Remove(Tree6, 1),
    T0 = Tree7,
    T1 = maps:put("Balance", 0, T0),
    Tree8 = T1,
    dump(Tree8, 0).

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
