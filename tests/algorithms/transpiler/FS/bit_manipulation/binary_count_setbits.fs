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
let rec binary_count_setbits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("Input value must be a non-negative integer")
        let mutable count: int = 0
        let mutable value: int = n
        while value > 0 do
            count <- count + (((value % 2 + 2) % 2))
            value <- value / 2
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (binary_count_setbits (25)))
printfn "%s" (_str (binary_count_setbits (36)))
printfn "%s" (_str (binary_count_setbits (16)))
printfn "%s" (_str (binary_count_setbits (58)))
printfn "%s" (_str (binary_count_setbits (0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
