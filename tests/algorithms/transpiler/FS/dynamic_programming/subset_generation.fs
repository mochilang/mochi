// Generated 2025-08-07 15:46 +0700

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
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec copy_list (src: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable src = src
    try
        let mutable result: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (src)) do
            result <- Array.append result [|_idx src (i)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec subset_combinations (elements: int array) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable elements = elements
    let mutable n = n
    try
        let r: int = Seq.length (elements)
        if n > r then
            __ret <- Array.empty<int array>
            raise Return
        let mutable dp: int array array array = [||]
        let mutable i: int = 0
        while i <= r do
            dp <- Array.append dp [|[||]|]
            i <- i + 1
        dp.[0] <- Array.append (_idx dp (0)) [|[||]|]
        i <- 1
        while i <= r do
            let mutable j: int = i
            while j > 0 do
                let mutable prevs: int array array = _idx dp (j - 1)
                let mutable k: int = 0
                while k < (Seq.length (prevs)) do
                    let mutable prev: int array = _idx prevs (k)
                    let mutable comb: int array = copy_list (prev)
                    comb <- Array.append comb [|_idx elements (i - 1)|]
                    dp.[j] <- Array.append (_idx dp (j)) [|comb|]
                    k <- k + 1
                j <- j - 1
            i <- i + 1
        __ret <- _idx dp (n)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (subset_combinations (unbox<int array> [|10; 20; 30; 40|]) (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
