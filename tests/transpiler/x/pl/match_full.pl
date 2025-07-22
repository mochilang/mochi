:- initialization(main).
:- style_check(-singleton).

classify(N, R) :-
    (N =:= 0 -> Return1 = "zero" ; (N =:= 1 -> Return1 = "one" ; Return1 = "many")),
    R = Return1.

main :-
    X is 2,
    (2 =:= 1 -> Label = "one" ; (2 =:= 2 -> Label = "two" ; (2 =:= 3 -> Label = "three" ; Label = "unknown"))),
    writeln(Label),
    Day = "sun",
    ("sun" = "mon" -> Mood = "tired" ; ("sun" = "fri" -> Mood = "excited" ; ("sun" = "sun" -> Mood = "relaxed" ; Mood = "normal"))),
    writeln(Mood),
    Ok = true,
    (true = true -> Status = "confirmed" ; (true = false -> Status = "denied" ; Status = 0)),
    writeln(Status),
    classify(0, R9), writeln(R9),
    classify(5, R10), writeln(R10).
