% chat-server.erl - generated from chat-server.mochi

removeName(Names, Name) ->
    Out0 = [],
    lists:foreach(fun(N) -> (case (N /= Name) of true -> Out1 = Out0 ++ [N]; _ -> ok end) end, Names),
    Out1.

main() ->
    Clients0 = [],
    Broadcast = fun(Msg) -> io:format("~p~n", [Msg]) end,
    Add = fun(Name) -> Clients1 = Clients0 ++ [Name], Broadcast("+++ \"" ++ Name ++ "\" connected +++\n") end,
    Send = fun(Name, Msg) -> Broadcast(Name ++ "> " ++ Msg ++ "\n") end,
    Remove = fun(Name) -> Clients2 = removeName(Clients1, Name), Broadcast("--- \"" ++ Name ++ "\" disconnected ---\n") end,
    Add("Alice"),
    Add("Bob"),
    Send("Alice", "Hello Bob!"),
    Send("Bob", "Hi Alice!"),
    Remove("Bob"),
    Remove("Alice"),
    Broadcast("Server stopping!\n").

main(_) ->
    main().
