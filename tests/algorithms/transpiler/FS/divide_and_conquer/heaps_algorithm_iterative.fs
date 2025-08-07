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
let rec copy_list (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable result: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            result <- Array.append result [|_idx arr (i)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec heaps (arr: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable arr = arr
    try
        if (Seq.length (arr)) <= 1 then
            let single: int array array = [||]
            __ret <- Array.append single [|copy_list (arr)|]
            raise Return
        let n: int = Seq.length (arr)
        let mutable c: int array = [||]
        let mutable i: int = 0
        while i < n do
            c <- Array.append c [|0|]
            i <- i + 1
        let mutable res: int array array = [||]
        res <- Array.append res [|copy_list (arr)|]
        i <- 0
        while i < n do
            if (_idx c (i)) < i then
                if (((i % 2 + 2) % 2)) = 0 then
                    let temp: int = _idx arr (0)
                    arr.[0] <- _idx arr (i)
                    arr.[i] <- temp
                else
                    let temp: int = _idx arr (_idx c (i))
                    arr.[_idx c (i)] <- _idx arr (i)
                    arr.[i] <- temp
                res <- Array.append res [|copy_list (arr)|]
                c.[i] <- (_idx c (i)) + 1
                i <- 0
            else
                c.[i] <- 0
                i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (heaps (unbox<int array> [|1; 2; 3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
