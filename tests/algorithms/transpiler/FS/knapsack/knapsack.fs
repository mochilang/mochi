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
        else
            let mutable left_capacity: int = capacity - (_idx weights (counter - 1))
            let mutable new_value_included: int = (_idx values (counter - 1)) + (knapsack (left_capacity) (weights) (values) (counter - 1))
            let mutable without_new_value: int = knapsack (capacity) (weights) (values) (counter - 1)
            if new_value_included > without_new_value then
                __ret <- new_value_included
                raise Return
            else
                __ret <- without_new_value
                raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable weights: int array = unbox<int array> [|10; 20; 30|]
        let mutable values: int array = unbox<int array> [|60; 100; 120|]
        let mutable cap: int = 50
        let mutable count: int = Seq.length (values)
        let mutable result: int = knapsack (cap) (weights) (values) (count)
        printfn "%s" (_str (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
