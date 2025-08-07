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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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

let test_graph_1: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 2|]); (1, [|0; 3|]); (2, [|0|]); (3, [|1|]); (4, [|5; 6|]); (5, [|4; 6|]); (6, [|4; 5|])]
let test_graph_2: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 2; 3|]); (1, [|0; 3|]); (2, [|0|]); (3, [|0; 1|]); (4, Array.empty<int>); (5, Array.empty<int>)]
let rec dfs (graph: System.Collections.Generic.IDictionary<int, int array>) (vert: int) (visited: bool array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable vert = vert
    let mutable visited = visited
    try
        visited.[vert] <- true
        let mutable connected_verts: int array = [||]
        for neighbour in _dictGet graph (vert) do
            if not (_idx visited (neighbour)) then
                connected_verts <- unbox<int array> (Array.append (connected_verts) (dfs (graph) (neighbour) (visited)))
        __ret <- unbox<int array> (Array.append ([|vert|]) (connected_verts))
        raise Return
        __ret
    with
        | Return -> __ret
let rec connected_components (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    try
        let graph_size: int = Seq.length (graph)
        let mutable visited: bool array = [||]
        for _ in 0 .. (graph_size - 1) do
            visited <- Array.append visited [|false|]
        let mutable components_list: int array array = [||]
        for i in 0 .. (graph_size - 1) do
            if not (_idx visited (i)) then
                let component: int array = dfs (graph) (i) (visited)
                components_list <- Array.append components_list [|component|]
        __ret <- components_list
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (connected_components (test_graph_1)))
printfn "%s" (_str (connected_components (test_graph_2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
