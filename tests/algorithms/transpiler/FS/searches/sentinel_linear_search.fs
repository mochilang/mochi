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
let rec remove_last (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec sentinel_linear_search (sequence: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sequence = sequence
    let mutable target = target
    try
        let mutable seq: int array = sequence
        seq <- Array.append seq [|target|]
        let mutable index: int = 0
        while (_idx seq (int index)) <> target do
            index <- index + 1
        seq <- remove_last (seq)
        if index = (Seq.length (seq)) then
            __ret <- -1
            raise Return
        __ret <- index
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (sentinel_linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (0)))
printfn "%s" (_str (sentinel_linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (15)))
printfn "%s" (_str (sentinel_linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (5)))
printfn "%s" (_str (sentinel_linear_search (unbox<int array> [|0; 5; 7; 10; 15|]) (6)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
