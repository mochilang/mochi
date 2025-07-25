// Generated 2025-07-26 05:05 +0700

exception Return

let rec angleDiff (b1: float) (b2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b1 = b1
    let mutable b2 = b2
    try
        let diff: float = b2 - b1
        __ret <- ((((((((diff % 360.0 + 360.0) % 360.0)) + 360.0) + 180.0) % 360.0 + 360.0) % 360.0)) - 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testCases: float array array = [|[|20.0; 45.0|]; [|(float 0) - 45.0; 45.0|]; [|(float 0) - 85.0; 90.0|]; [|(float 0) - 95.0; 90.0|]; [|(float 0) - 45.0; 125.0|]; [|(float 0) - 45.0; 145.0|]; [|29.4803; (float 0) - 88.6381|]; [|(float 0) - 78.3251; (float 0) - 159.036|]; [|(float 0) - 70099.74233810938; 29840.67437876723|]; [|(float 0) - 165313.6666297357; 33693.9894517456|]; [|1174.8380510598456; (float 0) - 154146.66490124757|]; [|60175.77306795546; 42213.07192354373|]|]
for tc in testCases do
    printfn "%A" (angleDiff (float (tc.[0])) (float (tc.[1])))
