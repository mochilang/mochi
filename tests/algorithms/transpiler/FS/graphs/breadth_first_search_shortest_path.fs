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
type Graph = {
    graph: System.Collections.Generic.IDictionary<string, string array>
    parent: System.Collections.Generic.IDictionary<string, string>
    source: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec newGraph (g: System.Collections.Generic.IDictionary<string, string array>) (s: string) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    let mutable s = s
    try
        __ret <- { graph = g; parent = _dictCreate []; source = s }
        raise Return
        __ret
    with
        | Return -> __ret
let rec breath_first_search (g: Graph) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    try
        let mutable parent: System.Collections.Generic.IDictionary<string, string> = g.parent
        parent.[g.source] <- g.source
        let mutable queue: string array = [|g.source|]
        let mutable idx: int = 0
        while idx < (Seq.length (queue)) do
            let vertex: string = _idx queue (idx)
            for adj in Seq.map string (_dictGet (g.graph) ((string (vertex)))) do
                if not (parent.ContainsKey(adj)) then
                    parent.[adj] <- vertex
                    queue <- Array.append queue [|adj|]
            idx <- idx + 1
        g <- { g with parent = parent }
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec shortest_path (g: Graph) (target: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable g = g
    let mutable target = target
    try
        if target = (g.source) then
            __ret <- g.source
            raise Return
        if not ((g.parent).ContainsKey(target)) then
            __ret <- (("No path from vertex: " + (g.source)) + " to vertex: ") + target
            raise Return
        let p: string = _dictGet (g.parent) ((string (target)))
        __ret <- ((shortest_path (g) (p)) + "->") + target
        raise Return
        __ret
    with
        | Return -> __ret
let graph: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("A", [|"B"; "C"; "E"|]); ("B", [|"A"; "D"; "E"|]); ("C", [|"A"; "F"; "G"|]); ("D", [|"B"|]); ("E", [|"A"; "B"; "D"|]); ("F", [|"C"|]); ("G", [|"C"|])]
let mutable g: Graph = newGraph (graph) ("G")
g <- breath_first_search (g)
printfn "%s" (shortest_path (g) ("D"))
printfn "%s" (shortest_path (g) ("G"))
printfn "%s" (shortest_path (g) ("Foo"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
