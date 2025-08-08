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
type GraphAdjacencyList = {
    mutable _adj_list: System.Collections.Generic.IDictionary<string, string array>
    mutable _directed: bool
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec make_graph (_directed: bool) =
    let mutable __ret : GraphAdjacencyList = Unchecked.defaultof<GraphAdjacencyList>
    let mutable _directed = _directed
    try
        let mutable m: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        __ret <- { _adj_list = m; _directed = _directed }
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_vertex (m: System.Collections.Generic.IDictionary<string, string array>) (v: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    let mutable v = v
    try
        __ret <- m.ContainsKey(v)
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_edge (g: GraphAdjacencyList) (s: string) (d: string) =
    let mutable __ret : GraphAdjacencyList = Unchecked.defaultof<GraphAdjacencyList>
    let mutable g = g
    let mutable s = s
    let mutable d = d
    try
        let mutable adj: System.Collections.Generic.IDictionary<string, string array> = g._adj_list
        if not (g._directed) then
            if (contains_vertex (adj) (s)) && (contains_vertex (adj) (d)) then
                adj.[s] <- Array.append (_dictGet adj ((string (s)))) [|d|]
                adj.[d] <- Array.append (_dictGet adj ((string (d)))) [|s|]
            else
                if contains_vertex (adj) (s) then
                    adj.[s] <- Array.append (_dictGet adj ((string (s)))) [|d|]
                    adj.[d] <- [|s|]
                else
                    if contains_vertex (adj) (d) then
                        adj.[d] <- Array.append (_dictGet adj ((string (d)))) [|s|]
                        adj.[s] <- [|d|]
                    else
                        adj.[s] <- [|d|]
                        adj.[d] <- [|s|]
        else
            if (contains_vertex (adj) (s)) && (contains_vertex (adj) (d)) then
                adj.[s] <- Array.append (_dictGet adj ((string (s)))) [|d|]
            else
                if contains_vertex (adj) (s) then
                    adj.[s] <- Array.append (_dictGet adj ((string (s)))) [|d|]
                    adj.[d] <- [||]
                else
                    if contains_vertex (adj) (d) then
                        adj.[s] <- [|d|]
                    else
                        adj.[s] <- [|d|]
                        adj.[d] <- [||]
        g._adj_list <- adj
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec graph_to_string (g: GraphAdjacencyList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable g = g
    try
        __ret <- _str (g._adj_list)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable d_graph: GraphAdjacencyList = make_graph (true)
d_graph <- add_edge (d_graph) (_str (0)) (_str (1))
printfn "%s" (graph_to_string (d_graph))
d_graph <- add_edge (d_graph) (_str (1)) (_str (2))
d_graph <- add_edge (d_graph) (_str (1)) (_str (4))
d_graph <- add_edge (d_graph) (_str (1)) (_str (5))
printfn "%s" (graph_to_string (d_graph))
d_graph <- add_edge (d_graph) (_str (2)) (_str (0))
d_graph <- add_edge (d_graph) (_str (2)) (_str (6))
d_graph <- add_edge (d_graph) (_str (2)) (_str (7))
printfn "%s" (graph_to_string (d_graph))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
