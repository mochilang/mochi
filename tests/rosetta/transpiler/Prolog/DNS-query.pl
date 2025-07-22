:- initialization(main).
:- style_check(-singleton).

main :-
    Net.LookupHost("www.kame.net", Res),
    Addrs = nth0(0, Res, R),
    Err = nth0(1, Res, R),
    (Err =:= Nil ->
    writeln(Addrs) ;
    writeln(Err)).
