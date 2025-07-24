// Generated 2025-07-24 20:52 +0700

exception Return

let rec angleDiff (b1: float) (b2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b1 = b1
    let mutable b2 = b2
    try
        let d: float = b2 - b1
        if d < (0 - 180.0) then
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
let mutable testCases: array array = [|[|20.0; 45.0|]; [|0 - 45.0; 45.0|]; [|0 - 85.0; 90.0|]; [|0 - 95.0; 90.0|]; [|0 - 45.0; 125.0|]; [|0 - 45.0; 145.0|]; [|29.4803; 0 - 88.6381|]; [|0 - 78.3251; 0 - 159.036|]|]
for tc in testCases do
    printfn "%A" (angleDiff (tc.[0]) (tc.[1]))
