// Generated 2025-08-06 15:56 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[").Replace("|]", "]").Replace("; ", " ").Replace(";", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec create_all_state (increment: int) (total: int) (level: int) (current: int array) (result: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable increment = increment
    let mutable total = total
    let mutable level = level
    let mutable current = current
    let mutable result = result
    try
        if level = 0 then
            __ret <- Array.append result [|current|]
            raise Return
        let mutable i: int = increment
        while i <= ((total - level) + 1) do
            let next_current: int array = Array.append current [|i|]
            result <- create_all_state (i + 1) (total) (level - 1) (next_current) (result)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and generate_all_combinations (n: int) (k: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    let mutable k = k
    try
        if (k < 0) || (n < 0) then
            __ret <- Array.empty<int array>
            raise Return
        let mutable result: int array array = [||]
        __ret <- create_all_state (1) (n) (k) (Array.empty<int>) (result)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (generate_all_combinations (4) (2)))
printfn "%s" (_str (generate_all_combinations (3) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
