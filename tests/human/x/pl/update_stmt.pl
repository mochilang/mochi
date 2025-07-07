:- initialization(main).
people([
  person('Alice',17,'minor'),
  person('Bob',25,'unknown'),
  person('Charlie',18,'unknown'),
  person('Diana',16,'minor')
]).
update([],[]).
update([person(N,A,S)|T],[person(N,A2,S2)|T2]) :-
    (A >= 18 -> S2='adult', A2 is A+1 ; S2=S, A2=A),
    update(T,T2).
main :-
    people(P), update(P,_Updated),
    writeln('ok'), halt.
