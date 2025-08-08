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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec knapsack (capacity: int) (weights: int array) (values: int array) (counter: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable capacity = capacity
    let mutable weights = weights
    let mutable values = values
    let mutable counter = counter
    try
        if (counter = 0) || (capacity = 0) then
            __ret <- 0
            raise Return
        if (_idx weights (counter - 1)) > capacity then
            __ret <- knapsack (capacity) (weights) (values) (counter - 1)
            raise Return
        let left_capacity: int = capacity - (_idx weights (counter - 1))
        let include_val: int = (_idx values (counter - 1)) + (knapsack (left_capacity) (weights) (values) (counter - 1))
        let exclude_val: int = knapsack (capacity) (weights) (values) (counter - 1)
        if include_val > exclude_val then
            __ret <- include_val
            raise Return
        __ret <- exclude_val
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_base_case () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let cap: int = 0
        let ``val``: int array = unbox<int array> [|0|]
        let w: int array = unbox<int array> [|0|]
        let c: int = Seq.length (``val``)
        if (knapsack (cap) (w) (``val``) (c)) <> 0 then
            __ret <- false
            raise Return
        let val2: int array = unbox<int array> [|60|]
        let w2: int array = unbox<int array> [|10|]
        let c2: int = Seq.length (val2)
        __ret <- (knapsack (cap) (w2) (val2) (c2)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_easy_case () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let cap: int = 3
        let ``val``: int array = unbox<int array> [|1; 2; 3|]
        let w: int array = unbox<int array> [|3; 2; 1|]
        let c: int = Seq.length (``val``)
        __ret <- (knapsack (cap) (w) (``val``) (c)) = 5
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_knapsack () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let cap: int = 50
        let ``val``: int array = unbox<int array> [|60; 100; 120|]
        let w: int array = unbox<int array> [|10; 20; 30|]
        let c: int = Seq.length (``val``)
        __ret <- (knapsack (cap) (w) (``val``) (c)) = 220
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (test_base_case())
printfn "%b" (test_easy_case())
printfn "%b" (test_knapsack())
printfn "%b" (true)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
