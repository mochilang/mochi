// Generated 2025-08-17 08:49 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec binomial_coefficient (n: int) (r: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable r = r
    try
        if (n < 0) || (r < 0) then
            ignore (failwith ("n and r must be non-negative integers"))
        if (n = 0) || (r = 0) then
            __ret <- 1
            raise Return
        let mutable c: int array = Array.empty<int>
        for _ in 0 .. ((r + 1) - 1) do
            c <- Array.append c [|0|]
        c.[0] <- 1
        let mutable i: int = 1
        while i <= n do
            let mutable j: int = if i < r then i else r
            while j > 0 do
                c.[j] <- (_idx c (int j)) + (_idx c (int (j - 1)))
                j <- j - 1
            i <- i + 1
        __ret <- _idx c (int r)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (binomial_coefficient (10) (5))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
