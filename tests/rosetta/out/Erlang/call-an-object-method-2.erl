% call-an-object-method-2.erl - generated from call-an-object-method-2.mochi

newFactory() ->
    Sn0 = 0,
    New = fun() -> Sn1 = (Sn0 + 1), B0 = #{"__name" => "Box", secret => Sn1}, (case (Sn1 == 1) of true -> B1 = B0#{Contents => "rabbit"}; _ -> (case (Sn1 == 2) of true -> B2 = B1#{Contents => "rock"}; _ -> ok end) end), B2 end,
    Count = fun() -> Sn1 end,
    [New, Count].

main(_) ->
    Funcs = newFactory(),
    New = lists:nth((0)+1, Funcs),
    Count = lists:nth((1)+1, Funcs).
