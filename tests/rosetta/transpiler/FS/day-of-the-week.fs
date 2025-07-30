// Generated 2025-07-31 00:10 +0700

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
let rec weekday (y: int) (m: int) (d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        let mutable yy: int = y
        let mutable mm: int = m
        if mm < 3 then
            mm <- mm + 12
            yy <- yy - 1
        let k: int = ((yy % 100 + 100) % 100)
        let j: int = int (yy / 100)
        let a: int = int ((13 * (mm + 1)) / 5)
        let b: int = int (k / 4)
        let c: int = int (j / 4)
        __ret <- (((((((d + a) + k) + b) + c) + (5 * j)) % 7 + 7) % 7)
        raise Return
        __ret
    with
        | Return -> __ret
for year in 2008 .. (2122 - 1) do
    if (int (weekday year 12 25)) = 1 then
        printfn "%s" (("25 December " + (string year)) + " is Sunday")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
