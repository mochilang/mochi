% algebraic-data-types.erl - generated from algebraic-data-types.mochi

node(Cl, Le, Aa, Ri) ->
    #{"cl" => Cl, "le" => Le, "aa" => Aa, "ri" => Ri}.

treeString(T) ->
    (case (T == undefined) of true -> "E"; _ -> ok end),
    M = T,
    "T(" ++ mochi_get("cl", M) ++ ", " ++ treeString(mochi_get("le", M)) ++ ", " ++ lists:flatten(io_lib:format("~p", [mochi_get("aa", M)])) ++ ", " ++ treeString(mochi_get("ri", M)) ++ ")".

balance(T) ->
    (case (T == undefined) of true -> T; _ -> ok end),
    M = T,
    (case (mochi_get("cl", M) /= "B") of true -> T; _ -> ok end),
    Le = mochi_get("le", M),
    Ri = mochi_get("ri", M),
    (case (Le /= undefined) of true -> LeMap = Le, (case (mochi_get("cl", LeMap) == "R") of true -> Lele = mochi_get("le", LeMap), (case (Lele /= undefined) of true -> LeleMap = Lele, (case (mochi_get("cl", LeleMap) == "R") of true -> node("R", node("B", mochi_get("le", LeleMap), mochi_get("aa", LeleMap), mochi_get("ri", LeleMap)), mochi_get("aa", LeMap), node("B", mochi_get("ri", LeMap), mochi_get("aa", M), Ri)); _ -> ok end); _ -> ok end), Leri = mochi_get("ri", LeMap), (case (Leri /= undefined) of true -> LeriMap = Leri, (case (mochi_get("cl", LeriMap) == "R") of true -> node("R", node("B", mochi_get("le", LeMap), mochi_get("aa", LeMap), mochi_get("le", LeriMap)), mochi_get("aa", LeriMap), node("B", mochi_get("ri", LeriMap), mochi_get("aa", M), Ri)); _ -> ok end); _ -> ok end); _ -> ok end); _ -> ok end),
    (case (Ri /= undefined) of true -> RiMap = Ri, (case (mochi_get("cl", RiMap) == "R") of true -> Rile = mochi_get("le", RiMap), (case (Rile /= undefined) of true -> RileMap = Rile, (case (mochi_get("cl", RileMap) == "R") of true -> node("R", node("B", mochi_get("le", M), mochi_get("aa", M), mochi_get("le", RileMap)), mochi_get("aa", RileMap), node("B", mochi_get("ri", RileMap), mochi_get("aa", RiMap), mochi_get("ri", RiMap))); _ -> ok end); _ -> ok end), Riri = mochi_get("ri", RiMap), (case (Riri /= undefined) of true -> RiriMap = Riri, (case (mochi_get("cl", RiriMap) == "R") of true -> node("R", node("B", mochi_get("le", M), mochi_get("aa", M), mochi_get("le", RiMap)), mochi_get("aa", RiMap), node("B", mochi_get("le", RiriMap), mochi_get("aa", RiriMap), mochi_get("ri", RiriMap))); _ -> ok end); _ -> ok end); _ -> ok end); _ -> ok end),
    T.

ins(Tr, X) ->
    (case (Tr == undefined) of true -> node("R", undefined, X, undefined); _ -> ok end),
    (case (X < mochi_get("aa", Tr)) of true -> balance(node(mochi_get("cl", Tr), ins(mochi_get("le", Tr), X), mochi_get("aa", Tr), mochi_get("ri", Tr))); _ -> ok end),
    (case (X > mochi_get("aa", Tr)) of true -> balance(node(mochi_get("cl", Tr), mochi_get("le", Tr), mochi_get("aa", Tr), ins(mochi_get("ri", Tr), X))); _ -> ok end),
    Tr.

insert(Tr, X) ->
    T = ins(Tr, X),
    (case (T == undefined) of true -> undefined; _ -> ok end),
    M = T,
    node("B", mochi_get("le", M), mochi_get("aa", M), mochi_get("ri", M)).

main(_) ->
    Tr0 = undefined,
    I0 = 1,
    (fun Loop0(I, Tr) -> case (I =< 16) of true -> Tr1 = insert(Tr, I), I1 = (I + 1), Loop0(Tr1, I1); _ -> ok end end(I0, Tr0)),
    io:format("~p~n", [treeString(Tr1)]).

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
