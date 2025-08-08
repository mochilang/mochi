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
type Graph = {
    mutable _adj: System.Collections.Generic.IDictionary<string, string array>
    mutable _directed: bool
}
open System.Collections.Generic

let rec create_graph (vertices: string array) (edges: string array array) (_directed: bool) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable vertices = vertices
    let mutable edges = edges
    let mutable _directed = _directed
    try
        let mutable _adj: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for v in Seq.map string (vertices) do
            _adj.[v] <- [||]
        for e in edges do
            let s: string = _idx e (0)
            let d: string = _idx e (1)
            if not (_adj.ContainsKey(s)) then
                _adj.[s] <- [||]
            if not (_adj.ContainsKey(d)) then
                _adj.[d] <- [||]
            _adj.[s] <- Array.append (_dictGet _adj ((string (s)))) [|d|]
            if not _directed then
                _adj.[d] <- Array.append (_dictGet _adj ((string (d)))) [|s|]
        __ret <- { _adj = _adj; _directed = _directed }
        raise Return
        __ret
    with
        | Return -> __ret
and add_vertex (graph: Graph) (v: string) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable graph = graph
    let mutable v = v
    try
        if (graph._adj).ContainsKey(v) then
            failwith ("vertex exists")
        let mutable _adj: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for k in (graph._adj).Keys do
            _adj.[k] <- _dictGet (graph._adj) ((string (k)))
        _adj.[v] <- [||]
        __ret <- { _adj = _adj; _directed = graph._directed }
        raise Return
        __ret
    with
        | Return -> __ret
and remove_from_list (lst: string array) (value: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lst = lst
    let mutable value = value
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) <> value then
                res <- Array.append res [|(_idx lst (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and remove_key (m: System.Collections.Generic.IDictionary<string, string array>) (key: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, string array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, string array>>
    let mutable m = m
    let mutable key = key
    try
        let mutable res: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for k in m.Keys do
            if k <> key then
                res.[k] <- _dictGet m ((string (k)))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and add_edge (graph: Graph) (s: string) (d: string) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable graph = graph
    let mutable s = s
    let mutable d = d
    try
        if (not ((graph._adj).ContainsKey(s))) || (not ((graph._adj).ContainsKey(d))) then
            failwith ("vertex missing")
        if contains_edge (graph) (s) (d) then
            failwith ("edge exists")
        let mutable _adj: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for k in (graph._adj).Keys do
            _adj.[k] <- _dictGet (graph._adj) ((string (k)))
        let mutable list_s: string array = _dictGet _adj ((string (s)))
        list_s <- Array.append list_s [|d|]
        _adj.[s] <- list_s
        if not (graph._directed) then
            let mutable list_d: string array = _dictGet _adj ((string (d)))
            list_d <- Array.append list_d [|s|]
            _adj.[d] <- list_d
        __ret <- { _adj = _adj; _directed = graph._directed }
        raise Return
        __ret
    with
        | Return -> __ret
and remove_edge (graph: Graph) (s: string) (d: string) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable graph = graph
    let mutable s = s
    let mutable d = d
    try
        if (not ((graph._adj).ContainsKey(s))) || (not ((graph._adj).ContainsKey(d))) then
            failwith ("vertex missing")
        if not (contains_edge (graph) (s) (d)) then
            failwith ("edge missing")
        let mutable _adj: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for k in (graph._adj).Keys do
            _adj.[k] <- _dictGet (graph._adj) ((string (k)))
        _adj.[s] <- remove_from_list (_dictGet _adj ((string (s)))) (d)
        if not (graph._directed) then
            _adj.[d] <- remove_from_list (_dictGet _adj ((string (d)))) (s)
        __ret <- { _adj = _adj; _directed = graph._directed }
        raise Return
        __ret
    with
        | Return -> __ret
and remove_vertex (graph: Graph) (v: string) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable graph = graph
    let mutable v = v
    try
        if not ((graph._adj).ContainsKey(v)) then
            failwith ("vertex missing")
        let mutable _adj: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for k in (graph._adj).Keys do
            if k <> v then
                _adj.[k] <- remove_from_list (_dictGet (graph._adj) ((string (k)))) (v)
        __ret <- { _adj = _adj; _directed = graph._directed }
        raise Return
        __ret
    with
        | Return -> __ret
and contains_vertex (graph: Graph) (v: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable v = v
    try
        __ret <- (graph._adj).ContainsKey(v)
        raise Return
        __ret
    with
        | Return -> __ret
and contains_edge (graph: Graph) (s: string) (d: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable s = s
    let mutable d = d
    try
        if (not ((graph._adj).ContainsKey(s))) || (not ((graph._adj).ContainsKey(d))) then
            failwith ("vertex missing")
        for x in Seq.map string (_dictGet (graph._adj) ((string (s)))) do
            if x = d then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and clear_graph (graph: Graph) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable graph = graph
    try
        __ret <- { _adj = _dictCreate []; _directed = graph._directed }
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (graph: Graph) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable graph = graph
    try
        __ret <- _str (graph._adj)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let vertices: string array = [|"1"; "2"; "3"; "4"|]
        let edges: string array array = [|[|"1"; "2"|]; [|"2"; "3"|]; [|"3"; "4"|]|]
        let mutable g: Graph = create_graph (vertices) (edges) (false)
        printfn "%s" (to_string (g))
        g <- add_vertex (g) ("5")
        g <- add_edge (g) ("4") ("5")
        printfn "%s" (_str (contains_edge (g) ("4") ("5")))
        g <- remove_edge (g) ("1") ("2")
        g <- remove_vertex (g) ("3")
        printfn "%s" (to_string (g))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
