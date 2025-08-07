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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec maximum_non_adjacent_sum (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            __ret <- 0
            raise Return
        let mutable max_including: int = _idx nums (0)
        let mutable max_excluding: int = 0
        let mutable i: int = 1
        while i < (Seq.length (nums)) do
            let num: int = _idx nums (i)
            let new_including: int = max_excluding + num
            let new_excluding: int = if max_including > max_excluding then max_including else max_excluding
            max_including <- new_including
            max_excluding <- new_excluding
            i <- i + 1
        if max_including > max_excluding then
            __ret <- max_including
            raise Return
        __ret <- max_excluding
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (maximum_non_adjacent_sum (unbox<int array> [|1; 2; 3|])))
printfn "%s" (_str (maximum_non_adjacent_sum (unbox<int array> [|1; 5; 3; 7; 2; 2; 6|])))
printfn "%s" (_str (maximum_non_adjacent_sum (unbox<int array> [|-1; -5; -3; -7; -2; -2; -6|])))
printfn "%s" (_str (maximum_non_adjacent_sum (unbox<int array> [|499; 500; -3; -7; -2; -2; -6|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
