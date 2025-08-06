// Generated 2025-08-06 20:48 +0700

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
let rec binary_count_trailing_zeros (a: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    try
        if a < 0 then
            failwith ("Input value must be a non-negative integer")
        if a = 0 then
            __ret <- 0
            raise Return
        let mutable n: int = a
        let mutable count: int = 0
        while (((n % 2 + 2) % 2)) = 0 do
            count <- count + 1
            n <- n / 2
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (binary_count_trailing_zeros (25)))
printfn "%s" (_str (binary_count_trailing_zeros (36)))
printfn "%s" (_str (binary_count_trailing_zeros (16)))
printfn "%s" (_str (binary_count_trailing_zeros (58)))
printfn "%s" (_str (binary_count_trailing_zeros (int 4294967296L)))
printfn "%s" (_str (binary_count_trailing_zeros (0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
