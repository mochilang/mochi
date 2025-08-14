// Generated 2025-08-14 16:04 +0700

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
type Edge = {
    mutable _u: int
    mutable _v: int
    mutable _w: int
}
type Graph = {
    mutable _num_nodes: int
    mutable _edges: Edge array
    mutable _component: System.Collections.Generic.IDictionary<int, int>
}
type UnionResult = {
    mutable _graph: Graph
    mutable _component_size: int array
}
open System.Collections.Generic

let rec new_graph (_num_nodes: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable _num_nodes = _num_nodes
    try
        __ret <- { _num_nodes = _num_nodes; _edges = Array.empty<Edge>; _component = _dictCreate<int, int> [] }
        raise Return
        __ret
    with
        | Return -> __ret
and add_edge (g: Graph) (_u: int) (_v: int) (_w: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    let mutable _u = _u
    let mutable _v = _v
    let mutable _w = _w
    try
        let mutable es: Edge array = g._edges
        es <- Array.append es [|{ _u = _u; _v = _v; _w = _w }|]
        __ret <- { _num_nodes = g._num_nodes; _edges = es; _component = g._component }
        raise Return
        __ret
    with
        | Return -> __ret
and find_component (g: Graph) (node: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable node = node
    try
        __ret <- if (_dictGet (g._component) (node)) = node then node else (find_component (g) (_dictGet (g._component) (node)))
        raise Return
        __ret
    with
        | Return -> __ret
and set_component (g: Graph) (node: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    let mutable node = node
    try
        if (_dictGet (g._component) (node)) <> node then
            let mutable comp: System.Collections.Generic.IDictionary<int, int> = g._component
            let mutable k: int = 0
            while k < (g._num_nodes) do
                comp <- _dictAdd (comp) (k) (find_component (g) (k))
                k <- k + 1
            g <- { _num_nodes = g._num_nodes; _edges = g._edges; _component = comp }
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and union (g: Graph) (_component_size: int array) (_u: int) (_v: int) =
    let mutable __ret : UnionResult = Unchecked.defaultof<UnionResult>
    let mutable g = g
    let mutable _component_size = _component_size
    let mutable _u = _u
    let mutable _v = _v
    try
        let mutable comp_size: int array = _component_size
        let mutable comp: System.Collections.Generic.IDictionary<int, int> = g._component
        if (_idx comp_size (int _u)) <= (_idx comp_size (int _v)) then
            comp <- _dictAdd (comp) (_u) (_v)
            comp_size.[_v] <- (_idx comp_size (int _v)) + (_idx comp_size (int _u))
            g <- { _num_nodes = g._num_nodes; _edges = g._edges; _component = comp }
            g <- set_component (g) (_u)
        else
            comp <- _dictAdd (comp) (_v) (_u)
            comp_size.[_u] <- (_idx comp_size (int _u)) + (_idx comp_size (int _v))
            g <- { _num_nodes = g._num_nodes; _edges = g._edges; _component = comp }
            g <- set_component (g) (_v)
        __ret <- { _graph = g; _component_size = comp_size }
        raise Return
        __ret
    with
        | Return -> __ret
and create_empty_edges (n: int) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable n = n
    try
        let mutable res: Edge array = Array.empty<Edge>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|{ _u = 0 - 1; _v = 0 - 1; _w = 0 - 1 }|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and boruvka (g: Graph) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    try
        let mutable _component_size: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (g._num_nodes) do
            _component_size <- Array.append _component_size [|1|]
            let mutable comp: System.Collections.Generic.IDictionary<int, int> = g._component
            comp <- _dictAdd (comp) (i) (i)
            g <- { _num_nodes = g._num_nodes; _edges = g._edges; _component = comp }
            i <- i + 1
        let mutable mst_weight: int = 0
        let mutable num_components: int = g._num_nodes
        let mutable minimum_weight_edge: Edge array = create_empty_edges (g._num_nodes)
        while num_components > 1 do
            for e in g._edges do
                let _u: int = e._u
                let _v: int = e._v
                let _w: int = e._w
                let u_comp: int = _dictGet (g._component) (_u)
                let v_comp: int = _dictGet (g._component) (_v)
                if u_comp <> v_comp then
                    let current_u: Edge = _idx minimum_weight_edge (int u_comp)
                    if ((current_u._u) = (0 - 1)) || ((current_u._w) > _w) then
                        minimum_weight_edge.[u_comp] <- { _u = _u; _v = _v; _w = _w }
                    let current_v: Edge = _idx minimum_weight_edge (int v_comp)
                    if ((current_v._u) = (0 - 1)) || ((current_v._w) > _w) then
                        minimum_weight_edge.[v_comp] <- { _u = _u; _v = _v; _w = _w }
            for e in minimum_weight_edge do
                if (e._u) <> (0 - 1) then
                    let _u: int = e._u
                    let _v: int = e._v
                    let _w: int = e._w
                    let u_comp: int = _dictGet (g._component) (_u)
                    let v_comp: int = _dictGet (g._component) (_v)
                    if u_comp <> v_comp then
                        mst_weight <- mst_weight + _w
                        let mutable res: UnionResult = union (g) (_component_size) (u_comp) (v_comp)
                        g <- res._graph
                        _component_size <- res._component_size
                        ignore (printfn "%s" (((("Added edge [" + (_str (_u))) + " - ") + (_str (_v))) + "]"))
                        ignore (printfn "%s" ("Added weight: " + (_str (_w))))
                        ignore (printfn "%s" (""))
                        num_components <- num_components - 1
            minimum_weight_edge <- create_empty_edges (g._num_nodes)
        ignore (printfn "%s" ("The total weight of the minimal spanning tree is: " + (_str (mst_weight))))
        __ret <- mst_weight
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable g: Graph = new_graph (8)
        let _edges: int array array = [|[|0; 1; 10|]; [|0; 2; 6|]; [|0; 3; 5|]; [|1; 3; 15|]; [|2; 3; 4|]; [|3; 4; 8|]; [|4; 5; 10|]; [|4; 6; 6|]; [|4; 7; 5|]; [|5; 7; 15|]; [|6; 7; 4|]|]
        for e in _edges do
            g <- add_edge (g) (_idx e (int 0)) (_idx e (int 1)) (_idx e (int 2))
        ignore (boruvka (g))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
