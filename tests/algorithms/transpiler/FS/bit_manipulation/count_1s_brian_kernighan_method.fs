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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec lowest_set_bit (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable lb: int = 1
        while (((n % (lb * 2) + (lb * 2)) % (lb * 2))) = 0 do
            lb <- lb * 2
        __ret <- lb
        raise Return
        __ret
    with
        | Return -> __ret
and get_1s_count (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        if number < 0 then
            printfn "%s" ("ValueError: Input must be a non-negative integer")
            __ret <- 0
            raise Return
        let mutable n: int = number
        let mutable count: int = 0
        while n > 0 do
            n <- n - (lowest_set_bit (n))
            count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (get_1s_count (25)))
printfn "%s" (_str (get_1s_count (37)))
printfn "%s" (_str (get_1s_count (21)))
printfn "%s" (_str (get_1s_count (58)))
printfn "%s" (_str (get_1s_count (0)))
printfn "%s" (_str (get_1s_count (256)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
