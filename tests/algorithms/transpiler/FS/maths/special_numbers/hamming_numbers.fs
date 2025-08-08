// Generated 2025-08-08 18:58 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec hamming (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 1 then
            failwith ("n_element should be a positive number")
        let mutable hamming_list: int array = unbox<int array> [|1|]
        let mutable i: int = 0
        let mutable j: int = 0
        let mutable k: int = 0
        let mutable index: int = 1
        while index < n do
            while ((int64 (_idx hamming_list (int i))) * (int64 2)) <= (int64 (_idx hamming_list (int ((Seq.length (hamming_list)) - 1)))) do
                i <- i + 1
            while ((int64 (_idx hamming_list (int j))) * (int64 3)) <= (int64 (_idx hamming_list (int ((Seq.length (hamming_list)) - 1)))) do
                j <- j + 1
            while ((int64 (_idx hamming_list (int k))) * (int64 5)) <= (int64 (_idx hamming_list (int ((Seq.length (hamming_list)) - 1)))) do
                k <- k + 1
            let m1: int = int ((int64 (_idx hamming_list (int i))) * (int64 2))
            let m2: int = int ((int64 (_idx hamming_list (int j))) * (int64 3))
            let m3: int = int ((int64 (_idx hamming_list (int k))) * (int64 5))
            let mutable next: int = m1
            if m2 < next then
                next <- m2
            if m3 < next then
                next <- m3
            hamming_list <- Array.append hamming_list [|next|]
            index <- index + 1
        __ret <- hamming_list
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (hamming (5)))
printfn "%s" (_repr (hamming (10)))
printfn "%s" (_repr (hamming (15)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
