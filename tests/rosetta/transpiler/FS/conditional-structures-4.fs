// Generated 2025-07-27 23:45 +0700

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
let rec fetchSomething () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec doPos (x: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    try

        __ret
    with
        | Return -> __ret
let rec doNeg (x: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    try

        __ret
    with
        | Return -> __ret
let rec example4 () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let x: int = fetchSomething()
        if x > 0 then
            doPos x
        else
            doNeg x
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
