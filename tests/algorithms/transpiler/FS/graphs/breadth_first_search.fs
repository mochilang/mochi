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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec add_edge (graph: System.Collections.Generic.IDictionary<int, int array>) (from: int) (``to``: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable graph = graph
    let mutable from = from
    let mutable ``to`` = ``to``
    try
        if graph.ContainsKey(from) then
            graph.[from] <- Array.append (_dictGet graph (from)) [|``to``|]
        else
            graph.[from] <- [|``to``|]
        __ret
    with
        | Return -> __ret
let rec print_graph (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable graph = graph
    try
        for v in graph.Keys do
            let adj: int array = _dictGet graph (v)
            let mutable line: string = (_str (v)) + "  :  "
            let mutable i: int = 0
            while i < (Seq.length (adj)) do
                line <- line + (_str (_idx adj (i)))
                if i < ((Seq.length (adj)) - 1) then
                    line <- line + " -> "
                i <- i + 1
            printfn "%s" (line)
        __ret
    with
        | Return -> __ret
let rec bfs (graph: System.Collections.Generic.IDictionary<int, int array>) (start: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable start = start
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable queue: int array = [||]
        let mutable order: int array = [||]
        queue <- Array.append queue [|start|]
        visited.[start] <- true
        let mutable head: int = 0
        while head < (Seq.length (queue)) do
            let vertex: int = _idx queue (head)
            head <- head + 1
            order <- Array.append order [|vertex|]
            let neighbors: int array = _dictGet graph (vertex)
            let mutable i: int = 0
            while i < (Seq.length (neighbors)) do
                let neighbor: int = _idx neighbors (i)
                if not (visited.ContainsKey(neighbor)) then
                    visited.[neighbor] <- true
                    queue <- Array.append queue [|neighbor|]
                i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
let mutable g: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
add_edge (g) (0) (1)
add_edge (g) (0) (2)
add_edge (g) (1) (2)
add_edge (g) (2) (0)
add_edge (g) (2) (3)
add_edge (g) (3) (3)
print_graph (g)
printfn "%s" (_repr (bfs (g) (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
