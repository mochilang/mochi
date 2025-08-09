// Generated 2025-08-09 15:58 +0700

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
let rec climb_stairs (number_of_steps: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number_of_steps = number_of_steps
    try
        if number_of_steps <= 0 then
            failwith ("number_of_steps needs to be positive")
        if number_of_steps = 1 then
            __ret <- 1
            raise Return
        let mutable previous: int = 1
        let mutable current: int = 1
        let mutable i: int = 0
        while i < (number_of_steps - 1) do
            let next: int = current + previous
            previous <- current
            current <- next
            i <- i + 1
        __ret <- current
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (climb_stairs (3))
printfn "%d" (climb_stairs (1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
