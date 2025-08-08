// Generated 2025-08-08 16:03 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec insertion_sort (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable i: int = 1
        while i < (Seq.length (a)) do
            let key: int = _idx a (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx a (j)) > key) do
                a.[j + 1] <- _idx a (j)
                j <- j - 1
            a.[j + 1] <- key
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
let rec minimum_waiting_time (queries: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable queries = queries
    try
        let n: int = Seq.length (queries)
        if (n = 0) || (n = 1) then
            __ret <- 0
            raise Return
        let sorted: int array = insertion_sort (queries)
        let mutable total: int = 0
        let mutable i: int = 0
        while i < n do
            total <- total + ((_idx sorted (i)) * ((n - i) - 1))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (minimum_waiting_time (unbox<int array> [|3; 2; 1; 2; 6|]))
printfn "%d" (minimum_waiting_time (unbox<int array> [|3; 2; 1|]))
printfn "%d" (minimum_waiting_time (unbox<int array> [|1; 2; 3; 4|]))
printfn "%d" (minimum_waiting_time (unbox<int array> [|5; 5; 5; 5|]))
printfn "%d" (minimum_waiting_time (Array.empty<int>))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
