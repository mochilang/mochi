% case-sensitivity-of-identifiers.erl - generated from case-sensitivity-of-identifiers.mochi

main() ->
    Pkg_dog0 = "Salt",
    Dog0 = "Pepper",
    Pkg_DOG0 = "Mustard",
    PackageSees = fun(D1, D2, D3) -> io:format("~p~n", ["Package sees: " ++ D1 ++ " " ++ D2 ++ " " ++ D3]), #{"pkg_dog" => true, "Dog" => true, "pkg_DOG" => true} end,
    D0 = PackageSees(Pkg_dog0, Dog0, Pkg_DOG0),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [length(D0)])) ++ " dogs.\n"]),
    Dog0 = "Benjamin",
    D1 = PackageSees(Pkg_dog0, Dog0, Pkg_DOG0),
    io:format("~p~n", ["Main sees:   " ++ Dog0 ++ " " ++ Dog0 ++ " " ++ Pkg_DOG0]),
    D2 = maps:put("dog", true, D1),
    D3 = maps:put("Dog", true, D2),
    D4 = maps:put("pkg_DOG", true, D3),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [length(D4)])) ++ " dogs.\n"]),
    Dog1 = "Samba",
    D5 = PackageSees(Pkg_dog0, Dog1, Pkg_DOG0),
    io:format("~p~n", ["Main sees:   " ++ Dog0 ++ " " ++ Dog1 ++ " " ++ Pkg_DOG0]),
    D6 = maps:put("dog", true, D5),
    D7 = maps:put("Dog", true, D6),
    D8 = maps:put("pkg_DOG", true, D7),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [length(D8)])) ++ " dogs.\n"]),
    DOG0 = "Bernie",
    D9 = PackageSees(Pkg_dog0, Dog1, Pkg_DOG0),
    io:format("~p~n", ["Main sees:   " ++ Dog0 ++ " " ++ Dog1 ++ " " ++ DOG0]),
    D10 = maps:put("dog", true, D9),
    D11 = maps:put("Dog", true, D10),
    D12 = maps:put("pkg_DOG", true, D11),
    D13 = maps:put("DOG", true, D12),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [length(D13)])) ++ " dogs."]).

main(_) ->
    main().
