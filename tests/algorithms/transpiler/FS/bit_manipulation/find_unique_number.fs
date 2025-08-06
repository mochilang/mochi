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
let rec bit_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = a
        let mutable ub: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (ua > 0) || (ub > 0) do
            let abit: int = ((ua % 2 + 2) % 2)
            let bbit: int = ((ub % 2 + 2) % 2)
            if ((abit = 1) && (bbit = 0)) || ((abit = 0) && (bbit = 1)) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and find_unique_number (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        if (Seq.length (arr)) = 0 then
            failwith ("input list must not be empty")
        let mutable result: int = 0
        for num in arr do
            result <- bit_xor (result) (num)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (find_unique_number (unbox<int array> [|1; 1; 2; 2; 3|])))
printfn "%s" (_str (find_unique_number (unbox<int array> [|4; 5; 4; 6; 6|])))
printfn "%s" (_str (find_unique_number (unbox<int array> [|7|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
