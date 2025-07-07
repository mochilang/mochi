:- initialization(main).
classify(0,'zero').
classify(1,'one').
classify(_, 'many').
main :-
    X=2,
    (X=:=1->L1='one'; X=:=2->L1='two'; X=:=3->L1='three'; L1='unknown'),
    writeln(L1),
    Day='sun',
    (Day='mon'->Mood='tired'; Day='fri'->Mood='excited'; Day='sun'->Mood='relaxed'; Mood='normal'),
    writeln(Mood),
    Ok=true,
    (Ok=true->Status='confirmed'; Status='denied'),
    writeln(Status),
    classify(0,R0), writeln(R0),
    classify(5,R1), writeln(R1),
    halt.
