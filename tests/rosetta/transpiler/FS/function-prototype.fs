// Generated 2025-08-01 23:54 +0700

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
let rec a () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try

        __ret
    with
        | Return -> __ret
and b (x: int) (y: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    let mutable y = y
    try

        __ret
    with
        | Return -> __ret
and c (nums: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable nums = nums
    try

        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
