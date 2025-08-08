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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec assign_ranks (data: float array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    try
        let mutable ranks: int array = Array.empty<int>
        let n: int = Seq.length (data)
        let mutable i: int = 0
        while i < n do
            let mutable rank: int = 1
            let mutable j: int = 0
            while j < n do
                if ((_idx data (int j)) < (_idx data (int i))) || (((_idx data (int j)) = (_idx data (int i))) && (j < i)) then
                    rank <- rank + 1
                j <- j + 1
            ranks <- Array.append ranks [|rank|]
            i <- i + 1
        __ret <- ranks
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_spearman_rank_correlation (var1: float array) (var2: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable var1 = var1
    let mutable var2 = var2
    try
        if (Seq.length (var1)) <> (Seq.length (var2)) then
            failwith ("Lists must have equal length")
        let n: int = Seq.length (var1)
        let rank1: int array = assign_ranks (var1)
        let rank2: int array = assign_ranks (var2)
        let mutable i: int = 0
        let mutable d_sq: float = 0.0
        while i < n do
            let diff: float = float ((_idx rank1 (int i)) - (_idx rank2 (int i)))
            d_sq <- d_sq + (diff * diff)
            i <- i + 1
        let n_f: float = float n
        __ret <- 1.0 - ((6.0 * d_sq) / (n_f * ((n_f * n_f) - 1.0)))
        raise Return
        __ret
    with
        | Return -> __ret
and test_spearman () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let x: float array = unbox<float array> [|1.0; 2.0; 3.0; 4.0; 5.0|]
        let y_inc: float array = unbox<float array> [|2.0; 4.0; 6.0; 8.0; 10.0|]
        if (calculate_spearman_rank_correlation (x) (y_inc)) <> 1.0 then
            failwith ("case1")
        let y_dec: float array = unbox<float array> [|5.0; 4.0; 3.0; 2.0; 1.0|]
        if (calculate_spearman_rank_correlation (x) (y_dec)) <> (-1.0) then
            failwith ("case2")
        let y_mix: float array = unbox<float array> [|5.0; 1.0; 2.0; 9.0; 5.0|]
        if (calculate_spearman_rank_correlation (x) (y_mix)) <> 0.6 then
            failwith ("case3")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_spearman()
        printfn "%s" (_str (calculate_spearman_rank_correlation (unbox<float array> [|1.0; 2.0; 3.0; 4.0; 5.0|]) (unbox<float array> [|2.0; 4.0; 6.0; 8.0; 10.0|])))
        printfn "%s" (_str (calculate_spearman_rank_correlation (unbox<float array> [|1.0; 2.0; 3.0; 4.0; 5.0|]) (unbox<float array> [|5.0; 4.0; 3.0; 2.0; 1.0|])))
        printfn "%s" (_str (calculate_spearman_rank_correlation (unbox<float array> [|1.0; 2.0; 3.0; 4.0; 5.0|]) (unbox<float array> [|5.0; 1.0; 2.0; 9.0; 5.0|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
