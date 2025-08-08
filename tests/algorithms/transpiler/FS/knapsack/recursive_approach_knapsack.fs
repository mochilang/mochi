// Generated 2025-08-08 16:34 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec knapsack (weights: int array) (values: int array) (number_of_items: int) (max_weight: int) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable weights = weights
    let mutable values = values
    let mutable number_of_items = number_of_items
    let mutable max_weight = max_weight
    let mutable index = index
    try
        if index = number_of_items then
            __ret <- 0
            raise Return
        let ans1: int = knapsack (weights) (values) (number_of_items) (max_weight) (index + 1)
        let mutable ans2: int = 0
        if (_idx weights (index)) <= max_weight then
            ans2 <- (_idx values (index)) + (knapsack (weights) (values) (number_of_items) (max_weight - (_idx weights (index))) (index + 1))
        if ans1 > ans2 then
            __ret <- ans1
            raise Return
        __ret <- ans2
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let w1: int array = unbox<int array> [|1; 2; 4; 5|]
        let v1: int array = unbox<int array> [|5; 4; 8; 6|]
        let r1: int = knapsack (w1) (v1) (4) (5) (0)
        printfn "%s" (_str (r1))
        let w2: int array = unbox<int array> [|3; 4; 5|]
        let v2: int array = unbox<int array> [|10; 9; 8|]
        let r2: int = knapsack (w2) (v2) (3) (25) (0)
        printfn "%s" (_str (r2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
