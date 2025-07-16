% call-a-function-11.erl - generated from call-a-function-11.mochi

zeroval(Ival) ->
    X0 = Ival,
    X1 = 0,
    X1.

zeroptr(Ref) ->
    Ref0 = lists:sublist(Ref, 0) ++ [0] ++ lists:nthtail((0)+1, Ref).

main() ->
    I0 = 1,
    io:format("~p~n", ["initial: " ++ lists:flatten(io_lib:format("~p", [I0]))]),
    Tmp = zeroval(I0),
    io:format("~p~n", ["zeroval: " ++ lists:flatten(io_lib:format("~p", [I0]))]),
    Box0 = [I0],
    zeroptr(Box0),
    I1 = lists:nth((0)+1, Box0),
    io:format("~p~n", ["zeroptr: " ++ lists:flatten(io_lib:format("~p", [I1]))]),
    io:format("~p~n", ["pointer: 0"]).

main(_) ->
    main().
