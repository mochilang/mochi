// Generated 2025-08-08 11:10 +0700

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
let rec calculation_span (price: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable price = price
    try
        let n: int = Seq.length (price)
        let mutable st: int array = Array.empty<int>
        let mutable span: int array = Array.empty<int>
        st <- Array.append st [|0|]
        span <- Array.append span [|1|]
        for i in 1 .. (n - 1) do
            while ((Seq.length (st)) > 0) && ((_idx price (_idx st ((Seq.length (st)) - 1))) <= (_idx price (i))) do
                st <- Array.sub st 0 (((Seq.length (st)) - 1) - 0)
            let s: int = if (Seq.length (st)) <= 0 then (i + 1) else (i - (_idx st ((Seq.length (st)) - 1)))
            span <- Array.append span [|s|]
            st <- Array.append st [|i|]
        __ret <- span
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_array (arr: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    try
        for i in 0 .. ((Seq.length (arr)) - 1) do
            printfn "%d" (_idx arr (i))
        __ret
    with
        | Return -> __ret
let price: int array = [|10; 4; 5; 90; 120; 80|]
let spans: int array = calculation_span (price)
print_array (spans)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
