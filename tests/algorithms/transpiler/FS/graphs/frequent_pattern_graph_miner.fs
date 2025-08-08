// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type NodesData = {
    mutable _map: System.Collections.Generic.IDictionary<string, string array>
    mutable keys: string array
}
type ClusterData = {
    mutable _clusters: System.Collections.Generic.IDictionary<int, string array>
    mutable _weights: int array
}
type GraphData = {
    mutable _edges: System.Collections.Generic.IDictionary<string, string array>
    mutable keys: string array
}
open System.Collections.Generic

let EDGE_ARRAY: string array array array = [|[|[|"ab"; "e1"|]; [|"ac"; "e3"|]; [|"ad"; "e5"|]; [|"bc"; "e4"|]; [|"bd"; "e2"|]; [|"be"; "e6"|]; [|"bh"; "e12"|]; [|"cd"; "e2"|]; [|"ce"; "e4"|]; [|"de"; "e1"|]; [|"df"; "e8"|]; [|"dg"; "e5"|]; [|"dh"; "e10"|]; [|"ef"; "e3"|]; [|"eg"; "e2"|]; [|"fg"; "e6"|]; [|"gh"; "e6"|]; [|"hi"; "e3"|]|]; [|[|"ab"; "e1"|]; [|"ac"; "e3"|]; [|"ad"; "e5"|]; [|"bc"; "e4"|]; [|"bd"; "e2"|]; [|"be"; "e6"|]; [|"cd"; "e2"|]; [|"de"; "e1"|]; [|"df"; "e8"|]; [|"ef"; "e3"|]; [|"eg"; "e2"|]; [|"fg"; "e6"|]|]; [|[|"ab"; "e1"|]; [|"ac"; "e3"|]; [|"bc"; "e4"|]; [|"bd"; "e2"|]; [|"de"; "e1"|]; [|"df"; "e8"|]; [|"dg"; "e5"|]; [|"ef"; "e3"|]; [|"eg"; "e2"|]; [|"eh"; "e12"|]; [|"fg"; "e6"|]; [|"fh"; "e10"|]; [|"gh"; "e6"|]|]; [|[|"ab"; "e1"|]; [|"ac"; "e3"|]; [|"bc"; "e4"|]; [|"bd"; "e2"|]; [|"bh"; "e12"|]; [|"cd"; "e2"|]; [|"df"; "e8"|]; [|"dh"; "e10"|]|]; [|[|"ab"; "e1"|]; [|"ac"; "e3"|]; [|"ad"; "e5"|]; [|"bc"; "e4"|]; [|"bd"; "e2"|]; [|"cd"; "e2"|]; [|"ce"; "e4"|]; [|"de"; "e1"|]; [|"df"; "e8"|]; [|"dg"; "e5"|]; [|"ef"; "e3"|]; [|"eg"; "e2"|]; [|"fg"; "e6"|]|]|]
let rec contains (lst: string array) (item: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable lst = lst
    let mutable item = item
    try
        for v in Seq.map string (lst) do
            if v = item then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and get_distinct_edge (edge_array: string array array array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable edge_array = edge_array
    try
        let mutable distinct: string array = [||]
        for row in edge_array do
            for item in row do
                let mutable e: string = _idx item (0)
                if not (contains (distinct) (e)) then
                    distinct <- Array.append distinct [|e|]
        __ret <- distinct
        raise Return
        __ret
    with
        | Return -> __ret
and get_bitcode (edge_array: string array array array) (de: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable edge_array = edge_array
    let mutable de = de
    try
        let mutable bitcode: string = ""
        let mutable i: int = 0
        try
            while i < (Seq.length (edge_array)) do
                try
                    let mutable found: bool = false
                    try
                        for item in _idx edge_array (i) do
                            try
                                if (_idx item (0)) = de then
                                    found <- true
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if found then
                        bitcode <- bitcode + "1"
                    else
                        bitcode <- bitcode + "0"
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- bitcode
        raise Return
        __ret
    with
        | Return -> __ret
and count_ones (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable c: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = "1" then
                c <- c + 1
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
and get_frequency_table (edge_array: string array array array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, string> array = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, string> array>
    let mutable edge_array = edge_array
    try
        let mutable distinct: string array = get_distinct_edge (edge_array)
        let mutable table: System.Collections.Generic.IDictionary<string, string> array = [||]
        for e in Seq.map string (distinct) do
            let bit: string = get_bitcode (edge_array) (e)
            let cnt: int = count_ones (bit)
            let mutable entry: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("edge", e); ("count", _str (cnt)); ("bit", bit)]
            table <- Array.append table [|entry|]
        let mutable i: int = 0
        while i < (Seq.length (table)) do
            let mutable max_i: int = i
            let mutable j: int = i + 1
            while j < (Seq.length (table)) do
                if (toi (_dictGet (_idx table (j)) ((string ("count"))))) > (toi (_dictGet (_idx table (max_i)) ((string ("count"))))) then
                    max_i <- j
                j <- j + 1
            let tmp: System.Collections.Generic.IDictionary<string, string> = _idx table (i)
            table.[i] <- _idx table (max_i)
            table.[max_i] <- tmp
            i <- i + 1
        __ret <- table
        raise Return
        __ret
    with
        | Return -> __ret
and get_nodes (freq_table: System.Collections.Generic.IDictionary<string, string> array) =
    let mutable __ret : NodesData = Unchecked.defaultof<NodesData>
    let mutable freq_table = freq_table
    try
        let mutable nodes: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        let mutable keys: string array = [||]
        for f in freq_table.Keys do
            let code: obj = box (((f :?> System.Collections.Generic.IDictionary<string, obj>).["bit"]))
            let edge: obj = box (((f :?> System.Collections.Generic.IDictionary<string, obj>).["edge"]))
            if nodes.ContainsKey(code) then
                nodes.[code] <- Array.append (_dictGet nodes ((string (code)))) [|(unbox<string> edge)|]
            else
                nodes.[code] <- [|edge|]
                keys <- Array.append keys [|(unbox<string> code)|]
        __ret <- { _map = nodes; keys = keys }
        raise Return
        __ret
    with
        | Return -> __ret
and get_cluster (nodes: NodesData) =
    let mutable __ret : ClusterData = Unchecked.defaultof<ClusterData>
    let mutable nodes = nodes
    try
        let mutable _clusters: System.Collections.Generic.IDictionary<int, string array> = _dictCreate []
        let mutable _weights: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (nodes.keys)) do
            let code: string = _idx (nodes.keys) (i)
            let wt: int = count_ones (code)
            if _clusters.ContainsKey(wt) then
                _clusters.[wt] <- Array.append (_dictGet _clusters (wt)) [|code|]
            else
                _clusters.[wt] <- [|code|]
                _weights <- Array.append _weights [|wt|]
            i <- i + 1
        __ret <- { _clusters = _clusters; _weights = _weights }
        raise Return
        __ret
    with
        | Return -> __ret
and get_support (_clusters: ClusterData) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable _clusters = _clusters
    try
        let mutable sup: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (_clusters._weights)) do
            let w: int = _idx (_clusters._weights) (i)
            sup <- Array.append sup [|(_floordiv (w * 100) (Seq.length (_clusters._weights)))|]
            i <- i + 1
        __ret <- sup
        raise Return
        __ret
    with
        | Return -> __ret
and contains_bits (a: string) (b: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        while i < (String.length (a)) do
            let c1: string = _substring a i (i + 1)
            let mutable c2: string = _substring b i (i + 1)
            if (c1 = "1") && (c2 <> "1") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and max_cluster_key (_clusters: ClusterData) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _clusters = _clusters
    try
        let mutable m: int = 0
        let mutable i: int = 0
        while i < (Seq.length (_clusters._weights)) do
            let w: int = _idx (_clusters._weights) (i)
            if w > m then
                m <- w
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and get_cluster_codes (_clusters: ClusterData) (wt: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable _clusters = _clusters
    let mutable wt = wt
    try
        __ret <- if (_clusters._clusters).ContainsKey(wt) then (_dictGet (_clusters._clusters) (wt)) else (Array.empty<string>)
        raise Return
        __ret
    with
        | Return -> __ret
and create_edge (nodes: NodesData) (graph: System.Collections.Generic.IDictionary<string, string array>) (gkeys: string array) (_clusters: ClusterData) (c1: int) (maxk: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable nodes = nodes
    let mutable graph = graph
    let mutable gkeys = gkeys
    let mutable _clusters = _clusters
    let mutable c1 = c1
    let mutable maxk = maxk
    try
        let mutable keys: string array = gkeys
        let codes1: string array = get_cluster_codes (_clusters) (c1)
        let mutable idx1: int = 0
        try
            while idx1 < (Seq.length (codes1)) do
                try
                    let i_code: string = _idx codes1 (idx1)
                    let mutable count: int = 0
                    let mutable c2: int = c1 + 1
                    try
                        while c2 <= maxk do
                            try
                                let codes2: string array = get_cluster_codes (_clusters) (c2)
                                let mutable j: int = 0
                                while j < (Seq.length (codes2)) do
                                    let j_code: string = _idx codes2 (j)
                                    if contains_bits (i_code) (j_code) then
                                        if graph.ContainsKey(i_code) then
                                            graph.[i_code] <- Array.append (_dictGet graph ((string (i_code)))) [|j_code|]
                                        else
                                            graph.[i_code] <- [|j_code|]
                                            if not (contains (keys) (i_code)) then
                                                keys <- Array.append keys [|i_code|]
                                        if not (contains (keys) (j_code)) then
                                            keys <- Array.append keys [|j_code|]
                                        count <- count + 1
                                    j <- j + 1
                                if count = 0 then
                                    c2 <- c2 + 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    idx1 <- idx1 + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- keys
        raise Return
        __ret
    with
        | Return -> __ret
and construct_graph (_clusters: ClusterData) (nodes: NodesData) =
    let mutable __ret : GraphData = Unchecked.defaultof<GraphData>
    let mutable _clusters = _clusters
    let mutable nodes = nodes
    try
        let maxk: int = max_cluster_key (_clusters)
        let top_codes: string array = get_cluster_codes (_clusters) (maxk)
        let mutable graph: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        let mutable keys: string array = [|"Header"|]
        graph.["Header"] <- [||]
        let mutable i: int = 0
        while i < (Seq.length (top_codes)) do
            let code: string = _idx top_codes (i)
            graph.["Header"] <- Array.append (_dictGet graph ((string ("Header")))) [|code|]
            graph.[code] <- [|"Header"|]
            keys <- Array.append keys [|code|]
            i <- i + 1
        let mutable c: int = 1
        while c < maxk do
            keys <- create_edge (nodes) (graph) (keys) (_clusters) (c) (maxk)
            c <- c + 1
        __ret <- { _edges = graph; keys = keys }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable paths: string array array = [||]
let rec copy_list (lst: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lst = lst
    try
        let mutable n: string array = [||]
        for v in Seq.map string (lst) do
            n <- Array.append n [|v|]
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and my_dfs (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) (``end``: string) (path: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable graph = graph
    let mutable start = start
    let mutable ``end`` = ``end``
    let mutable path = path
    try
        let mutable new_path: string array = copy_list (path)
        new_path <- Array.append new_path [|start|]
        if start = ``end`` then
            paths <- Array.append paths [|new_path|]
            __ret <- ()
            raise Return
        for node in Seq.map string (_dictGet graph ((string (start)))) do
            let mutable seen: bool = false
            for p in Seq.map string (new_path) do
                if p = node then
                    seen <- true
            if not seen then
                my_dfs (graph) (node) (``end``) (new_path)
        __ret
    with
        | Return -> __ret
and find_freq_subgraph_given_support (s: int) (_clusters: ClusterData) (graph: GraphData) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable _clusters = _clusters
    let mutable graph = graph
    try
        let mutable k: int = _floordiv (s * (Seq.length (_clusters._weights))) 100
        let codes: string array = get_cluster_codes (_clusters) (k)
        let mutable i: int = 0
        while i < (Seq.length (codes)) do
            my_dfs (graph._edges) (_idx codes (i)) ("Header") (Array.empty<string>)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and node_edges (nodes: NodesData) (code: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable nodes = nodes
    let mutable code = code
    try
        __ret <- _dictGet (nodes._map) ((string (code)))
        raise Return
        __ret
    with
        | Return -> __ret
and freq_subgraphs_edge_list (paths: string array array) (nodes: NodesData) =
    let mutable __ret : string array array array = Unchecked.defaultof<string array array array>
    let mutable paths = paths
    let mutable nodes = nodes
    try
        let mutable freq_sub_el: string array array array = [||]
        for path in paths do
            let mutable el: string array array = [||]
            let mutable j: int = 0
            while j < ((Seq.length (path)) - 1) do
                let code: string = _idx path (j)
                let edge_list: string array = node_edges (nodes) (code)
                let mutable e: int = 0
                while e < (Seq.length (edge_list)) do
                    let edge: string = _idx edge_list (e)
                    let a: string = _substring edge 0 1
                    let b: string = _substring edge 1 2
                    el <- Array.append el [|[|a; b|]|]
                    e <- e + 1
                j <- j + 1
            freq_sub_el <- Array.append freq_sub_el [|el|]
        __ret <- freq_sub_el
        raise Return
        __ret
    with
        | Return -> __ret
and print_all (nodes: NodesData) (support: int array) (_clusters: ClusterData) (graph: GraphData) (freq_subgraph_edge_list: string array array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable nodes = nodes
    let mutable support = support
    let mutable _clusters = _clusters
    let mutable graph = graph
    let mutable freq_subgraph_edge_list = freq_subgraph_edge_list
    try
        printfn "%s" ("\nNodes\n")
        let mutable i: int = 0
        while i < (Seq.length (nodes.keys)) do
            let code: string = _idx (nodes.keys) (i)
            printfn "%s" (code)
            printfn "%s" (_repr (_dictGet (nodes._map) ((string (code)))))
            i <- i + 1
        printfn "%s" ("\nSupport\n")
        printfn "%s" (_repr (support))
        printfn "%s" ("\nCluster\n")
        let mutable j: int = 0
        while j < (Seq.length (_clusters._weights)) do
            let w: int = _idx (_clusters._weights) (j)
            printfn "%s" (((_str (w)) + ":") + (_str (_dictGet (_clusters._clusters) (w))))
            j <- j + 1
        printfn "%s" ("\nGraph\n")
        let mutable k: int = 0
        while k < (Seq.length (graph.keys)) do
            let key: string = _idx (graph.keys) (k)
            printfn "%s" (key)
            printfn "%s" (_repr (_dictGet (graph._edges) ((string (key)))))
            k <- k + 1
        printfn "%s" ("\nEdge List of Frequent subgraphs\n")
        for el in freq_subgraph_edge_list do
            printfn "%s" (_repr (el))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let frequency_table: System.Collections.Generic.IDictionary<string, string> array = get_frequency_table (EDGE_ARRAY)
        let nodes: NodesData = get_nodes (frequency_table)
        let _clusters: ClusterData = get_cluster (nodes)
        let support: int array = get_support (_clusters)
        let graph: GraphData = construct_graph (_clusters) (nodes)
        find_freq_subgraph_given_support (60) (_clusters) (graph)
        let freq_subgraph_edge_list: string array array array = freq_subgraphs_edge_list (paths) (nodes)
        print_all (nodes) (support) (_clusters) (graph) (freq_subgraph_edge_list)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
