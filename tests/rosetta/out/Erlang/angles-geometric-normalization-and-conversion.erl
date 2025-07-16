% angles-geometric-normalization-and-conversion.erl - generated from angles-geometric-normalization-and-conversion.mochi

d2d(D) ->
    rem(D, 360).

g2g(G) ->
    rem(G, 400).

m2m(M) ->
    rem(M, 6400).

r2r(R) ->
    rem(R, ((2 * 3.141592653589793))).

d2g(D) ->
    ((d2d(D) * 400) / 360).

d2m(D) ->
    ((d2d(D) * 6400) / 360).

d2r(D) ->
    ((d2d(D) * 3.141592653589793) / 180).

g2d(G) ->
    ((g2g(G) * 360) / 400).

g2m(G) ->
    ((g2g(G) * 6400) / 400).

g2r(G) ->
    ((g2g(G) * 3.141592653589793) / 200).

m2d(M) ->
    ((m2m(M) * 360) / 6400).

m2g(M) ->
    ((m2m(M) * 400) / 6400).

m2r(M) ->
    ((m2m(M) * 3.141592653589793) / 3200).

r2d(R) ->
    ((r2r(R) * 180) / 3.141592653589793).

r2g(R) ->
    ((r2r(R) * 200) / 3.141592653589793).

r2m(R) ->
    ((r2r(R) * 3200) / 3.141592653589793).

main() ->
    Angles = [-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795, 359, 399, 6399, 1e+06],
    io:format("~p~n", ["degrees normalized_degs gradians mils radians"]),
    lists:foreach(fun(A) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " " ++ lists:flatten(io_lib:format("~p", [d2d(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [d2g(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [d2m(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [d2r(A)]))]) end, Angles),
    io:format("~p~n", ["\ngradians normalized_grds degrees mils radians"]),
    lists:foreach(fun(A) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " " ++ lists:flatten(io_lib:format("~p", [g2g(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [g2d(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [g2m(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [g2r(A)]))]) end, Angles),
    io:format("~p~n", ["\nmils normalized_mils degrees gradians radians"]),
    lists:foreach(fun(A) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " " ++ lists:flatten(io_lib:format("~p", [m2m(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [m2d(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [m2g(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [m2r(A)]))]) end, Angles),
    io:format("~p~n", ["\nradians normalized_rads degrees gradians mils"]),
    lists:foreach(fun(A) -> io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ " " ++ lists:flatten(io_lib:format("~p", [r2r(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [r2d(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [r2g(A)])) ++ " " ++ lists:flatten(io_lib:format("~p", [r2m(A)]))]) end, Angles).

main(_) ->
    main().
