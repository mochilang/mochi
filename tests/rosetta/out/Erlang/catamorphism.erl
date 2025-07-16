% catamorphism.erl - generated from catamorphism.mochi

add(A, B) ->
    (A + B).

sub(A, B) ->
    (A - B).

mul(A, B) ->
    (A * B).

fold(F, Xs) ->
    R0 = lists:nth((0)+1, Xs),
    I0 = 1,
    (fun Loop0(R, I) -> case (I < length(Xs)) of true -> R1 = f(R, lists:nth((I)+1, Xs)), I1 = (I + 1), Loop0(R1, I1); _ -> ok end end(R0, I0)),
    R1.

main(_) ->
    N = [1, 2, 3, 4, 5],
    io:format("~p~n", [fold(fun(A, B) -> add(A, B) end, N)]),
    io:format("~p~n", [fold(fun(A, B) -> sub(A, B) end, N)]),
    io:format("~p~n", [fold(fun(A, B) -> mul(A, B) end, N)]).
