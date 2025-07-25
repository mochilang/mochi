// Generated 2025-07-26 05:05 +0700

exception Return

let rec angleDiff (b1: float) (b2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b1 = b1
    let mutable b2 = b2
    try
        let d: float = b2 - b1
        if d < ((float 0) - 180.0) then
            __ret <- d + 360.0
            raise Return
        if d > 180.0 then
            __ret <- d - 360.0
            raise Return
        __ret <- d
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testCases: float array array = [|[|20.0; 45.0|]; [|(float 0) - 45.0; 45.0|]; [|(float 0) - 85.0; 90.0|]; [|(float 0) - 95.0; 90.0|]; [|(float 0) - 45.0; 125.0|]; [|(float 0) - 45.0; 145.0|]; [|29.4803; (float 0) - 88.6381|]; [|(float 0) - 78.3251; (float 0) - 159.036|]|]
for tc in testCases do
    printfn "%A" (angleDiff (float (tc.[0])) (float (tc.[1])))
