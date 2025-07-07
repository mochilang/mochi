:- initialization(main).
people([
  person{name:'Alice', age:30},
  person{name:'Bob', age:15},
  person{name:'Charlie', age:65},
  person{name:'Diana', age:45}
]).

is_adult(P) :- P.age >= 18.

add_senior_flag(P, person{name:Name, age:Age, is_senior:Senior}) :-
    Name = P.name,
    Age = P.age,
    (Age >= 60 -> Senior = true ; Senior = false).

main :-
    people(People),
    include(is_adult, People, Adults0),
    maplist(add_senior_flag, Adults0, Adults),
    writeln('--- Adults ---'),
    forall(member(P, Adults),
           (Name=P.name, Age=P.age,
            (P.is_senior -> S=' (senior)' ; S=''),
            format('~w is ~w~w~n', [Name, Age, S]))),
    halt.
