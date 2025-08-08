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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec contains (xs: int array) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable v = v
    try
        for x in xs do
            if x = v then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_edges (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable edges: int array array = [||]
        for i in 0 .. (n - 1) do
            for j in (_dictGet graph (i)).Keys do
                edges <- Array.append edges [|[|i; j|]|]
        __ret <- edges
        raise Return
        __ret
    with
        | Return -> __ret
let rec matching_min_vertex_cover (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    try
        let mutable chosen: int array = [||]
        let mutable edges: int array array = get_edges (graph)
        while (Seq.length (edges)) > 0 do
            let idx: int = (Seq.length (edges)) - 1
            let e: int array = _idx edges (idx)
            edges <- Array.sub edges 0 (idx - 0)
            let u: int = _idx e (0)
            let v: int = _idx e (1)
            if not (contains (chosen) (u)) then
                chosen <- Array.append chosen [|u|]
            if not (contains (chosen) (v)) then
                chosen <- Array.append chosen [|v|]
            let mutable filtered: int array array = [||]
            for edge in edges do
                let a: int = _idx edge (0)
                let b: int = _idx edge (1)
                if (((a <> u) && (b <> u)) && (a <> v)) && (b <> v) then
                    filtered <- Array.append filtered [|edge|]
            edges <- filtered
        __ret <- chosen
        raise Return
        __ret
    with
        | Return -> __ret
let graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 3|]); (1, [|0; 3|]); (2, [|0; 3; 4|]); (3, [|0; 1; 2|]); (4, [|2; 3|])]
let cover: int array = matching_min_vertex_cover (graph)
printfn "%s" (_str (cover))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
