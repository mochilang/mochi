// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
type KnapsackResult = {
    mutable _max_value: float
    mutable _fractions: float array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sort_by_ratio (index: int array) (ratio: float array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable index = index
    let mutable ratio = ratio
    try
        let mutable i: int = 1
        while i < (Seq.length (index)) do
            let key: int = _idx index (i)
            let key_ratio: float = _idx ratio (key)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx ratio (_idx index (j))) < key_ratio) do
                index.[j + 1] <- _idx index (j)
                j <- j - 1
            index.[j + 1] <- key
            i <- i + 1
        __ret <- index
        raise Return
        __ret
    with
        | Return -> __ret
let rec fractional_knapsack (value: float array) (weight: float array) (capacity: float) =
    let mutable __ret : KnapsackResult = Unchecked.defaultof<KnapsackResult>
    let mutable value = value
    let mutable weight = weight
    let mutable capacity = capacity
    try
        let n: int = Seq.length (value)
        let mutable index: int array = [||]
        let mutable i: int = 0
        while i < n do
            index <- Array.append index [|i|]
            i <- i + 1
        let mutable ratio: float array = [||]
        i <- 0
        while i < n do
            ratio <- Array.append ratio [|((_idx value (i)) / (_idx weight (i)))|]
            i <- i + 1
        index <- sort_by_ratio (index) (ratio)
        let mutable _fractions: float array = [||]
        i <- 0
        while i < n do
            _fractions <- Array.append _fractions [|0.0|]
            i <- i + 1
        let mutable _max_value: float = 0.0
        let mutable idx: int = 0
        try
            while idx < (Seq.length (index)) do
                try
                    let item: int = _idx index (idx)
                    if (_idx weight (item)) <= capacity then
                        _fractions.[item] <- 1.0
                        _max_value <- _max_value + (_idx value (item))
                        capacity <- capacity - (_idx weight (item))
                    else
                        _fractions.[item] <- capacity / (_idx weight (item))
                        _max_value <- _max_value + (((_idx value (item)) * capacity) / (_idx weight (item)))
                        raise Break
                    idx <- idx + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _max_value = _max_value; _fractions = _fractions }
        raise Return
        __ret
    with
        | Return -> __ret
let v: float array = [|1.0; 3.0; 5.0; 7.0; 9.0|]
let w: float array = [|0.9; 0.7; 0.5; 0.3; 0.1|]
printfn "%A" (fractional_knapsack (v) (w) (5.0))
printfn "%A" (fractional_knapsack (unbox<float array> [|1.0; 3.0; 5.0; 7.0|]) (unbox<float array> [|0.9; 0.7; 0.5; 0.3|]) (30.0))
printfn "%A" (fractional_knapsack (Array.empty<float>) (Array.empty<float>) (30.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
