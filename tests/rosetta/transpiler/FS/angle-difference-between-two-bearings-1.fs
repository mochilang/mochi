// Generated 2025-07-26 05:05 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
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
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
