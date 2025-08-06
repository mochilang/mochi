// Generated 2025-08-06 22:14 +0700

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
let rec xor_gate (input_1: int) (input_2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable input_1 = input_1
    let mutable input_2 = input_2
    try
        let mutable zeros: int = 0
        if input_1 = 0 then
            zeros <- zeros + 1
        if input_2 = 0 then
            zeros <- zeros + 1
        __ret <- ((zeros % 2 + 2) % 2)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (xor_gate (0) (0))
printfn "%d" (xor_gate (0) (1))
printfn "%d" (xor_gate (1) (0))
printfn "%d" (xor_gate (1) (1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
