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
type DirectedGraph = {
    mutable graph: System.Collections.Generic.IDictionary<int, int array array>
}
type Graph = {
    mutable graph: System.Collections.Generic.IDictionary<int, int array array>
}
open System

open System.Collections.Generic

let rec list_contains_int (xs: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and edge_exists (edges: int array array) (w: int) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable edges = edges
    let mutable w = w
    let mutable v = v
    try
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            if ((_idx (_idx edges (i)) (0)) = w) && ((_idx (_idx edges (i)) (1)) = v) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and first_key (m: System.Collections.Generic.IDictionary<int, int array array>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    try
        for k in m.Keys do
            __ret <- k
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and rand_range (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable low = low
    let mutable high = high
    try
        __ret <- int ((int ((((_now()) % (high - low) + (high - low)) % (high - low)))) + low)
        raise Return
        __ret
    with
        | Return -> __ret
and dg_make_graph () =
    let mutable __ret : DirectedGraph = Unchecked.defaultof<DirectedGraph>
    try
        __ret <- { graph = _dictCreate [] }
        raise Return
        __ret
    with
        | Return -> __ret
and dg_add_pair (g: DirectedGraph) (u: int) (v: int) (w: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    let mutable w = w
    try
        if (g.graph).ContainsKey(u) then
            let mutable edges: int array array = _dictGet (g.graph) (u)
            if not (edge_exists (edges) (w) (v)) then
                edges <- Array.append edges [|[|w; v|]|]
                let mutable m: System.Collections.Generic.IDictionary<int, int array array> = g.graph
                m.[u] <- edges
                g.graph <- m
        else
            let mutable m0: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m0.[u] <- [|[|w; v|]|]
            g.graph <- m0
        if not ((g.graph).ContainsKey(v)) then
            let mutable m1: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m1.[v] <- [||]
            g.graph <- m1
        __ret
    with
        | Return -> __ret
and dg_remove_pair (g: DirectedGraph) (u: int) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        if (g.graph).ContainsKey(u) then
            let mutable edges: int array array = _dictGet (g.graph) (u)
            let mutable new_edges: int array array = [||]
            let mutable i: int = 0
            while i < (Seq.length (edges)) do
                if (_idx (_idx edges (i)) (1)) <> v then
                    new_edges <- Array.append new_edges [|(_idx edges (i))|]
                i <- i + 1
            let mutable m: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m.[u] <- new_edges
            g.graph <- m
        __ret
    with
        | Return -> __ret
and dg_all_nodes (g: DirectedGraph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let mutable res: int array = [||]
        for k in (g.graph).Keys do
            res <- Array.append res [|k|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and dg_dfs_util (g: DirectedGraph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (order: int array) (d: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable order = order
    let mutable d = d
    try
        visited.[node] <- true
        order <- Array.append order [|node|]
        if (d <> (-1)) && (node = d) then
            __ret <- order
            raise Return
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                order <- dg_dfs_util (g) (neigh) (visited) (order) (d)
                if (d <> (-1)) && ((_idx order ((Seq.length (order)) - 1)) = d) then
                    __ret <- order
                    raise Return
            i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and dg_dfs (g: DirectedGraph) (s: int) (d: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable s = s
    let mutable d = d
    try
        if s = d then
            __ret <- Array.empty<int>
            raise Return
        let start: int = if s = (-2) then (first_key (g.graph)) else s
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable order: int array = [||]
        order <- dg_dfs_util (g) (start) (visited) (order) (d)
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and dg_bfs (g: DirectedGraph) (s: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable s = s
    try
        let mutable queue: int array = [||]
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable order: int array = [||]
        let start: int = if s = (-2) then (first_key (g.graph)) else s
        queue <- Array.append queue [|start|]
        visited.[start] <- true
        while (Seq.length (queue)) > 0 do
            let node: int = _idx queue (0)
            queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
            order <- Array.append order [|node|]
            let mutable edges: int array array = _dictGet (g.graph) (node)
            let mutable i: int = 0
            while i < (Seq.length (edges)) do
                let neigh: int = _idx (_idx edges (i)) (1)
                if not (visited.ContainsKey(neigh)) then
                    queue <- Array.append queue [|neigh|]
                    visited.[neigh] <- true
                i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and dg_in_degree (g: DirectedGraph) (u: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable u = u
    try
        let mutable count: int = 0
        for k in (g.graph).Keys do
            let mutable edges: int array array = _dictGet (g.graph) (k)
            let mutable i: int = 0
            while i < (Seq.length (edges)) do
                if (_idx (_idx edges (i)) (1)) = u then
                    count <- count + 1
                i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and dg_out_degree (g: DirectedGraph) (u: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable u = u
    try
        __ret <- if (g.graph).ContainsKey(u) then (Seq.length (_dictGet (g.graph) (u))) else 0
        raise Return
        __ret
    with
        | Return -> __ret
and dg_topo_util (g: DirectedGraph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (stack: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable stack = stack
    try
        visited.[node] <- true
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                stack <- dg_topo_util (g) (neigh) (visited) (stack)
            i <- i + 1
        stack <- Array.append stack [|node|]
        __ret <- stack
        raise Return
        __ret
    with
        | Return -> __ret
and dg_topological_sort (g: DirectedGraph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable stack: int array = [||]
        for k in (g.graph).Keys do
            if not (visited.ContainsKey(k)) then
                stack <- dg_topo_util (g) (k) (visited) (stack)
        let mutable res: int array = [||]
        let mutable i: int = (Seq.length (stack)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx stack (i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and dg_cycle_util (g: DirectedGraph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (``rec``: System.Collections.Generic.IDictionary<int, bool>) (res: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable ``rec`` = ``rec``
    let mutable res = res
    try
        visited.[node] <- true
        ``rec``.[node] <- true
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                res <- dg_cycle_util (g) (neigh) (visited) (``rec``) (res)
            else
                if _dictGet ``rec`` (neigh) then
                    if not (list_contains_int (res) (neigh)) then
                        res <- Array.append res [|neigh|]
                    if not (list_contains_int (res) (node)) then
                        res <- Array.append res [|node|]
            i <- i + 1
        ``rec``.[node] <- false
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and dg_cycle_nodes (g: DirectedGraph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable ``rec``: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable res: int array = [||]
        for k in (g.graph).Keys do
            if not (visited.ContainsKey(k)) then
                res <- dg_cycle_util (g) (k) (visited) (``rec``) (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and dg_has_cycle_util (g: DirectedGraph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (``rec``: System.Collections.Generic.IDictionary<int, bool>) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable ``rec`` = ``rec``
    try
        visited.[node] <- true
        ``rec``.[node] <- true
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                if dg_has_cycle_util (g) (neigh) (visited) (``rec``) then
                    __ret <- true
                    raise Return
            else
                if _dictGet ``rec`` (neigh) then
                    __ret <- true
                    raise Return
            i <- i + 1
        ``rec``.[node] <- false
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and dg_has_cycle (g: DirectedGraph) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable ``rec``: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        for k in (g.graph).Keys do
            if not (visited.ContainsKey(k)) then
                if dg_has_cycle_util (g) (k) (visited) (``rec``) then
                    __ret <- true
                    raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and dg_fill_graph_randomly (g: DirectedGraph) (c: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable c = c
    try
        let mutable count: int = c
        if count = (-1) then
            count <- rand_range (10) (10010)
        let mutable i: int = 0
        while i < count do
            let mutable edge_count: int = rand_range (1) (103)
            let mutable j: int = 0
            while j < edge_count do
                let n: int = rand_range (0) (count)
                if n <> i then
                    dg_add_pair (g) (i) (n) (1)
                j <- j + 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
and dg_dfs_time (g: DirectedGraph) (s: int) (e: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable s = s
    let mutable e = e
    try
        let ``begin``: int = _now()
        dg_dfs (g) (s) (e)
        let ``end``: int = _now()
        __ret <- ``end`` - ``begin``
        raise Return
        __ret
    with
        | Return -> __ret
and dg_bfs_time (g: DirectedGraph) (s: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable s = s
    try
        let ``begin``: int = _now()
        dg_bfs (g) (s)
        let ``end``: int = _now()
        __ret <- ``end`` - ``begin``
        raise Return
        __ret
    with
        | Return -> __ret
and g_make_graph () =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    try
        __ret <- { graph = _dictCreate [] }
        raise Return
        __ret
    with
        | Return -> __ret
and g_add_pair (g: Graph) (u: int) (v: int) (w: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    let mutable w = w
    try
        if (g.graph).ContainsKey(u) then
            let mutable edges: int array array = _dictGet (g.graph) (u)
            if not (edge_exists (edges) (w) (v)) then
                edges <- Array.append edges [|[|w; v|]|]
                let mutable m: System.Collections.Generic.IDictionary<int, int array array> = g.graph
                m.[u] <- edges
                g.graph <- m
        else
            let mutable m0: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m0.[u] <- [|[|w; v|]|]
            g.graph <- m0
        if (g.graph).ContainsKey(v) then
            let mutable edges2: int array array = _dictGet (g.graph) (v)
            if not (edge_exists (edges2) (w) (u)) then
                edges2 <- Array.append edges2 [|[|w; u|]|]
                let mutable m2: System.Collections.Generic.IDictionary<int, int array array> = g.graph
                m2.[v] <- edges2
                g.graph <- m2
        else
            let mutable m3: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m3.[v] <- [|[|w; u|]|]
            g.graph <- m3
        __ret
    with
        | Return -> __ret
and g_remove_pair (g: Graph) (u: int) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        if (g.graph).ContainsKey(u) then
            let mutable edges: int array array = _dictGet (g.graph) (u)
            let mutable new_edges: int array array = [||]
            let mutable i: int = 0
            while i < (Seq.length (edges)) do
                if (_idx (_idx edges (i)) (1)) <> v then
                    new_edges <- Array.append new_edges [|(_idx edges (i))|]
                i <- i + 1
            let mutable m: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m.[u] <- new_edges
            g.graph <- m
        if (g.graph).ContainsKey(v) then
            let mutable edges2: int array array = _dictGet (g.graph) (v)
            let mutable new_edges2: int array array = [||]
            let mutable j: int = 0
            while j < (Seq.length (edges2)) do
                if (_idx (_idx edges2 (j)) (1)) <> u then
                    new_edges2 <- Array.append new_edges2 [|(_idx edges2 (j))|]
                j <- j + 1
            let mutable m2: System.Collections.Generic.IDictionary<int, int array array> = g.graph
            m2.[v] <- new_edges2
            g.graph <- m2
        __ret
    with
        | Return -> __ret
and g_all_nodes (g: Graph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let mutable res: int array = [||]
        for k in (g.graph).Keys do
            res <- Array.append res [|k|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and g_dfs_util (g: Graph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (order: int array) (d: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable order = order
    let mutable d = d
    try
        visited.[node] <- true
        order <- Array.append order [|node|]
        if (d <> (-1)) && (node = d) then
            __ret <- order
            raise Return
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                order <- g_dfs_util (g) (neigh) (visited) (order) (d)
                if (d <> (-1)) && ((_idx order ((Seq.length (order)) - 1)) = d) then
                    __ret <- order
                    raise Return
            i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and g_dfs (g: Graph) (s: int) (d: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable s = s
    let mutable d = d
    try
        if s = d then
            __ret <- Array.empty<int>
            raise Return
        let start: int = if s = (-2) then (first_key (g.graph)) else s
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable order: int array = [||]
        order <- g_dfs_util (g) (start) (visited) (order) (d)
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and g_bfs (g: Graph) (s: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable s = s
    try
        let mutable queue: int array = [||]
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable order: int array = [||]
        let start: int = if s = (-2) then (first_key (g.graph)) else s
        queue <- Array.append queue [|start|]
        visited.[start] <- true
        while (Seq.length (queue)) > 0 do
            let node: int = _idx queue (0)
            queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
            order <- Array.append order [|node|]
            let mutable edges: int array array = _dictGet (g.graph) (node)
            let mutable i: int = 0
            while i < (Seq.length (edges)) do
                let neigh: int = _idx (_idx edges (i)) (1)
                if not (visited.ContainsKey(neigh)) then
                    queue <- Array.append queue [|neigh|]
                    visited.[neigh] <- true
                i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and g_degree (g: Graph) (u: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable u = u
    try
        __ret <- if (g.graph).ContainsKey(u) then (Seq.length (_dictGet (g.graph) (u))) else 0
        raise Return
        __ret
    with
        | Return -> __ret
and g_cycle_util (g: Graph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (parent: int) (res: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable parent = parent
    let mutable res = res
    try
        visited.[node] <- true
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                res <- g_cycle_util (g) (neigh) (visited) (node) (res)
            else
                if neigh <> parent then
                    if not (list_contains_int (res) (neigh)) then
                        res <- Array.append res [|neigh|]
                    if not (list_contains_int (res) (node)) then
                        res <- Array.append res [|node|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and g_cycle_nodes (g: Graph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable res: int array = [||]
        for k in (g.graph).Keys do
            if not (visited.ContainsKey(k)) then
                res <- g_cycle_util (g) (k) (visited) (-1) (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and g_has_cycle_util (g: Graph) (node: int) (visited: System.Collections.Generic.IDictionary<int, bool>) (parent: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable node = node
    let mutable visited = visited
    let mutable parent = parent
    try
        visited.[node] <- true
        let mutable edges: int array array = _dictGet (g.graph) (node)
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let neigh: int = _idx (_idx edges (i)) (1)
            if not (visited.ContainsKey(neigh)) then
                if g_has_cycle_util (g) (neigh) (visited) (node) then
                    __ret <- true
                    raise Return
            else
                if neigh <> parent then
                    __ret <- true
                    raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and g_has_cycle (g: Graph) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        for k in (g.graph).Keys do
            if not (visited.ContainsKey(k)) then
                if g_has_cycle_util (g) (k) (visited) (-1) then
                    __ret <- true
                    raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and g_fill_graph_randomly (g: Graph) (c: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable c = c
    try
        let mutable count: int = c
        if count = (-1) then
            count <- rand_range (10) (10010)
        let mutable i: int = 0
        while i < count do
            let mutable edge_count: int = rand_range (1) (103)
            let mutable j: int = 0
            while j < edge_count do
                let n: int = rand_range (0) (count)
                if n <> i then
                    g_add_pair (g) (i) (n) (1)
                j <- j + 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
and g_dfs_time (g: Graph) (s: int) (e: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable s = s
    let mutable e = e
    try
        let ``begin``: int = _now()
        g_dfs (g) (s) (e)
        let ``end``: int = _now()
        __ret <- ``end`` - ``begin``
        raise Return
        __ret
    with
        | Return -> __ret
and g_bfs_time (g: Graph) (s: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable s = s
    try
        let ``begin``: int = _now()
        g_bfs (g) (s)
        let ``end``: int = _now()
        __ret <- ``end`` - ``begin``
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable dg: DirectedGraph = dg_make_graph()
        dg_add_pair (dg) (0) (1) (5)
        dg_add_pair (dg) (0) (2) (3)
        dg_add_pair (dg) (1) (3) (2)
        dg_add_pair (dg) (2) (3) (4)
        printfn "%s" (_str (dg_dfs (dg) (-2) (-1)))
        printfn "%s" (_str (dg_bfs (dg) (-2)))
        printfn "%s" (_str (dg_in_degree (dg) (3)))
        printfn "%s" (_str (dg_out_degree (dg) (0)))
        printfn "%s" (_str (dg_topological_sort (dg)))
        printfn "%s" (_str (dg_has_cycle (dg)))
        let mutable ug: Graph = g_make_graph()
        g_add_pair (ug) (0) (1) (1)
        g_add_pair (ug) (1) (2) (1)
        g_add_pair (ug) (2) (0) (1)
        printfn "%s" (_str (g_dfs (ug) (-2) (-1)))
        printfn "%s" (_str (g_bfs (ug) (-2)))
        printfn "%s" (_str (g_degree (ug) (1)))
        printfn "%s" (_str (g_has_cycle (ug)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
