// Generated 2025-07-24 20:52 +0700

exception Return

let rec angleDiff (b1: float) (b2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b1 = b1
    let mutable b2 = b2
    try
        let diff: float = b2 - b1
        __ret <- ((((diff % 360.0) + 360.0) + 180.0) % 360.0) - 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testCases: array array = [|[|20.0; 45.0|]; [|0 - 45.0; 45.0|]; [|0 - 85.0; 90.0|]; [|0 - 95.0; 90.0|]; [|0 - 45.0; 125.0|]; [|0 - 45.0; 145.0|]; [|29.4803; 0 - 88.6381|]; [|0 - 78.3251; 0 - 159.036|]; [|0 - 70099.74233810938; 29840.67437876723|]; [|0 - 165313.6666297357; 33693.9894517456|]; [|1174.8380510598456; 0 - 154146.66490124757|]; [|60175.77306795546; 42213.07192354373|]|]
for tc in testCases do
    printfn "%A" (angleDiff (tc.[0]) (tc.[1]))
