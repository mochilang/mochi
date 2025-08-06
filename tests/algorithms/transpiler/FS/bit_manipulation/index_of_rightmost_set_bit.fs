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
let rec index_of_rightmost_set_bit (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        if number < 0 then
            failwith ("Input must be a non-negative integer")
        if number = 0 then
            __ret <- -1
            raise Return
        let mutable n: int = number
        let mutable index: int = 0
        while (((n % 2 + 2) % 2)) = 0 do
            n <- n / 2
            index <- index + 1
        __ret <- index
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (index_of_rightmost_set_bit (0)))
printfn "%s" (_str (index_of_rightmost_set_bit (5)))
printfn "%s" (_str (index_of_rightmost_set_bit (36)))
printfn "%s" (_str (index_of_rightmost_set_bit (8)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
