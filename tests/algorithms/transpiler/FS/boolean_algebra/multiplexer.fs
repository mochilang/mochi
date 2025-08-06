// Generated 2025-08-06 21:33 +0700

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
let rec mux (input0: int) (input1: int) (select: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable input0 = input0
    let mutable input1 = input1
    let mutable select = select
    try
        if (((input0 <> 0) && (input0 <> 1)) || ((input1 <> 0) && (input1 <> 1))) || ((select <> 0) && (select <> 1)) then
            failwith ("Inputs and select signal must be 0 or 1")
        if select = 1 then
            __ret <- input1
            raise Return
        __ret <- input0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (mux (0) (1) (0))
printfn "%d" (mux (0) (1) (1))
printfn "%d" (mux (1) (0) (0))
printfn "%d" (mux (1) (0) (1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
