// Generated 2025-08-17 12:28 +0700

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
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec isqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        while ((r + 1) * (r + 1)) <= n do
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and prime_sieve (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        if num <= 0 then
            ignore (failwith ("Invalid input, please enter a positive integer."))
        let mutable sieve: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i <= num do
            sieve <- Array.append sieve [|true|]
            i <- i + 1
        let mutable prime: int array = Array.empty<int>
        let mutable start: int = 2
        let ``end``: int = isqrt (num)
        while start <= ``end`` do
            if _idx sieve (int start) then
                prime <- Array.append prime [|start|]
                let mutable j: int = start * start
                while j <= num do
                    if _idx sieve (int j) then
                        sieve.[j] <- false
                    j <- j + start
            start <- start + 1
        let mutable k: int = ``end`` + 1
        while k <= num do
            if _idx sieve (int k) then
                prime <- Array.append prime [|k|]
            k <- k + 1
        __ret <- prime
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (prime_sieve (50))))
ignore (printfn "%s" (_str (prime_sieve (25))))
ignore (printfn "%s" (_str (prime_sieve (10))))
ignore (printfn "%s" (_str (prime_sieve (9))))
ignore (printfn "%s" (_str (prime_sieve (2))))
ignore (printfn "%s" (_str (prime_sieve (1))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
