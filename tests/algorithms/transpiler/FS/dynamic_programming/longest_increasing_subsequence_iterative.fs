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
let rec copy_list (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and longest_subsequence (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable lis: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable single: int array = [||]
            single <- Array.append single [|_idx arr (i)|]
            lis <- Array.append lis [|single|]
            i <- i + 1
        i <- 1
        while i < n do
            let mutable prev: int = 0
            while prev < i do
                if ((_idx arr (prev)) <= (_idx arr (i))) && (((Seq.length (_idx lis (prev))) + 1) > (Seq.length (_idx lis (i)))) then
                    let temp: int array = copy_list (_idx lis (prev))
                    let temp2: int array = Array.append temp [|_idx arr (i)|]
                    lis.[i] <- temp2
                prev <- prev + 1
            i <- i + 1
        let mutable result: int array = [||]
        i <- 0
        while i < n do
            if (Seq.length (_idx lis (i))) > (Seq.length (result)) then
                result <- _idx lis (i)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (longest_subsequence (unbox<int array> [|10; 22; 9; 33; 21; 50; 41; 60; 80|])))
        printfn "%s" (_str (longest_subsequence (unbox<int array> [|4; 8; 7; 5; 1; 12; 2; 3; 9|])))
        printfn "%s" (_str (longest_subsequence (unbox<int array> [|9; 8; 7; 6; 5; 7|])))
        printfn "%s" (_str (longest_subsequence (unbox<int array> [|28; 26; 12; 23; 35; 39|])))
        printfn "%s" (_str (longest_subsequence (unbox<int array> [|1; 1; 1|])))
        printfn "%s" (_str (longest_subsequence (Array.empty<int>)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
