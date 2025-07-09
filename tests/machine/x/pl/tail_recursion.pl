:- style_check(-singleton).
Sum_rec(N, Acc, _Res) :-
    ((N =:= 0) ->
        _Res = Acc.
    ; true
    ),
    Sum_rec((N - 1), (Acc + N), _V0),
    _Res = _V0.

:- initialization(main, main).
main :-
    Sum_rec(10, 0, _V0),
    writeln(_V0),
    true.
