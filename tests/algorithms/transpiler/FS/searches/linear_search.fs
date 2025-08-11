// Generated 2025-08-11 16:20 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec linear_search (sequence: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sequence = sequence
    let mutable target = target
    try
        let mutable i: int = 0
        while i < (Seq.length (sequence)) do
            if (_idx sequence (int i)) = target then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec rec_linear_search (sequence: int array) (low: int) (high: int) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sequence = sequence
    let mutable low = low
    let mutable high = high
    let mutable target = target
    try
        if not ((((0 <= high) && (high < (Seq.length (sequence)))) && (0 <= low)) && (low < (Seq.length (sequence)))) then
            failwith ("Invalid upper or lower bound!")
        if high < low then
            __ret <- -1
            raise Return
        if (_idx sequence (int low)) = target then
            __ret <- low
            raise Return
        if (_idx sequence (int high)) = target then
            __ret <- high
            raise Return
        __ret <- rec_linear_search (sequence) (low + 1) (high - 1) (target)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (0)))
printfn "%s" (_str (linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (15)))
printfn "%s" (_str (linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (5)))
printfn "%s" (_str (linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (6)))
printfn "%s" (_str (rec_linear_search (unbox<int array> [|0; 30; 500; 100; 700|]) (0) (4) (0)))
printfn "%s" (_str (rec_linear_search (unbox<int array> [|0; 30; 500; 100; 700|]) (0) (4) (700)))
printfn "%s" (_str (rec_linear_search (unbox<int array> [|0; 30; 500; 100; 700|]) (0) (4) (30)))
printfn "%s" (_str (rec_linear_search (unbox<int array> [|0; 30; 500; 100; 700|]) (0) (4) (-6)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
