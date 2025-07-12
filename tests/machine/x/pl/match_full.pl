:- style_check(-singleton).
classify(N, _Res) :-
    (N == 1 -> _V0 = "one" ; _V0 = "many"),
    (N == 0 -> _V1 = "zero" ; _V1 = _V0),
    _Res = _V1.

:- initialization(main, main).
main :-
    X is 2,
    (X == 3 -> _V0 = "three" ; _V0 = "unknown"),
    (X == 2 -> _V1 = "two" ; _V1 = _V0),
    (X == 1 -> _V2 = "one" ; _V2 = _V1),
    Label = _V2,
    writeln(Label),
    Day = "sun",
    (Day == "sun" -> _V3 = "relaxed" ; _V3 = "normal"),
    (Day == "fri" -> _V4 = "excited" ; _V4 = _V3),
    (Day == "mon" -> _V5 = "tired" ; _V5 = _V4),
    Mood = _V5,
    writeln(Mood),
    Ok = true,
    (Ok == true -> _V6 = "confirmed" ; _V6 = "denied"),
    Status = _V6,
    writeln(Status),
    classify(0, _V7),
    writeln(_V7),
    classify(5, _V8),
    writeln(_V8),
    true.
