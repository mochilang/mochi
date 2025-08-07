// Generated 2025-08-07 16:27 +0700

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

let rec is_bipartite_bfs (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        for node in graph.Keys do
            if not (visited.ContainsKey(node)) then
                let mutable queue: int array = [||]
                queue <- Array.append queue [|node|]
                visited.[node] <- 0
                while (Seq.length (queue)) > 0 do
                    let curr: int = _idx queue (0)
                    queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
                    for neighbor in _dictGet graph (curr) do
                        if not (visited.ContainsKey(neighbor)) then
                            visited.[neighbor] <- 1 - (_dictGet visited (curr))
                            queue <- Array.append queue [|neighbor|]
                        else
                            if (_dictGet visited (neighbor)) = (_dictGet visited (curr)) then
                                __ret <- false
                                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 3|]); (1, [|0; 2|]); (2, [|1; 3|]); (3, [|0; 2|])]
printfn "%s" (_str (is_bipartite_bfs (graph)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
