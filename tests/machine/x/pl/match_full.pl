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
    write(Label),
    nl,
    Day = "sun",
    (Day == "sun" -> _V3 = "relaxed" ; _V3 = "normal"),
    (Day == "fri" -> _V4 = "excited" ; _V4 = _V3),
    (Day == "mon" -> _V5 = "tired" ; _V5 = _V4),
    Mood = _V5,
    write(Mood),
    nl,
    Ok = true,
    (Ok == true -> _V6 = "confirmed" ; _V6 = "denied"),
    Status = _V6,
    write(Status),
    nl,
    classify(0, _V7),
    write(_V7),
    nl,
    classify(5, _V8),
    write(_V8),
    nl,
    true.
