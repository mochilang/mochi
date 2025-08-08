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
type Graph = {
    mutable _directed: bool
    mutable _vertex_to_index: System.Collections.Generic.IDictionary<int, int>
    mutable _adj_matrix: int array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec make_graph (vertices: int array) (edges: int array array) (_directed: bool) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable vertices = vertices
    let mutable edges = edges
    let mutable _directed = _directed
    try
        let mutable g: Graph = { _directed = _directed; _vertex_to_index = _dictCreate []; _adj_matrix = [||] }
        let mutable i: int = 0
        while i < (Seq.length (vertices)) do
            add_vertex (g) (_idx vertices (i))
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (edges)) do
            let e: int array = _idx edges (j)
            add_edge (g) (_idx e (0)) (_idx e (1))
            j <- j + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_vertex (g: Graph) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable v = v
    try
        __ret <- (g._vertex_to_index).ContainsKey(v)
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_vertex (g: Graph) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable v = v
    try
        if contains_vertex (g) (v) then
            failwith ("vertex already exists")
        let mutable matrix: int array array = g._adj_matrix
        let mutable i: int = 0
        while i < (Seq.length (matrix)) do
            matrix.[i] <- Array.append (_idx matrix (i)) [|0|]
            i <- i + 1
        let mutable row: int array = [||]
        let mutable j: int = 0
        while j < ((Seq.length (matrix)) + 1) do
            row <- Array.append row [|0|]
            j <- j + 1
        matrix <- Array.append matrix [|row|]
        g._adj_matrix <- matrix
        let mutable idx_map: System.Collections.Generic.IDictionary<int, int> = g._vertex_to_index
        idx_map.[v] <- (Seq.length (matrix)) - 1
        g._vertex_to_index <- idx_map
        __ret
    with
        | Return -> __ret
let rec remove_key (m: System.Collections.Generic.IDictionary<int, int>) (k: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int>>
    let mutable m = m
    let mutable k = k
    try
        let mutable out: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        for key in m.Keys do
            if key <> k then
                out.[key] <- _dictGet m (key)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrement_indices (m: System.Collections.Generic.IDictionary<int, int>) (start: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int>>
    let mutable m = m
    let mutable start = start
    try
        let mutable out: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        for key in m.Keys do
            let idx: int = _dictGet m (key)
            if idx > start then
                out.[key] <- idx - 1
            else
                out.[key] <- idx
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_vertex (g: Graph) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable v = v
    try
        if not (contains_vertex (g) (v)) then
            failwith ("vertex does not exist")
        let idx: int = _dictGet (g._vertex_to_index) (v)
        let mutable new_matrix: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (g._adj_matrix)) do
            if i <> idx then
                let mutable row: int array = _idx (g._adj_matrix) (i)
                let mutable new_row: int array = [||]
                let mutable j: int = 0
                while j < (Seq.length (row)) do
                    if j <> idx then
                        new_row <- Array.append new_row [|(_idx row (j))|]
                    j <- j + 1
                new_matrix <- Array.append new_matrix [|new_row|]
            i <- i + 1
        g._adj_matrix <- new_matrix
        let mutable m: System.Collections.Generic.IDictionary<int, int> = remove_key (g._vertex_to_index) (v)
        g._vertex_to_index <- decrement_indices (m) (idx)
        __ret
    with
        | Return -> __ret
let rec add_edge (g: Graph) (u: int) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        if not ((contains_vertex (g) (u)) && (contains_vertex (g) (v))) then
            failwith ("missing vertex")
        let mutable i: int = _dictGet (g._vertex_to_index) (u)
        let mutable j: int = _dictGet (g._vertex_to_index) (v)
        let mutable matrix: int array array = g._adj_matrix
        matrix.[i].[j] <- 1
        if not (g._directed) then
            matrix.[j].[i] <- 1
        g._adj_matrix <- matrix
        __ret
    with
        | Return -> __ret
let rec remove_edge (g: Graph) (u: int) (v: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        if not ((contains_vertex (g) (u)) && (contains_vertex (g) (v))) then
            failwith ("missing vertex")
        let mutable i: int = _dictGet (g._vertex_to_index) (u)
        let mutable j: int = _dictGet (g._vertex_to_index) (v)
        let mutable matrix: int array array = g._adj_matrix
        matrix.[i].[j] <- 0
        if not (g._directed) then
            matrix.[j].[i] <- 0
        g._adj_matrix <- matrix
        __ret
    with
        | Return -> __ret
let rec contains_edge (g: Graph) (u: int) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        if not ((contains_vertex (g) (u)) && (contains_vertex (g) (v))) then
            failwith ("missing vertex")
        let mutable i: int = _dictGet (g._vertex_to_index) (u)
        let mutable j: int = _dictGet (g._vertex_to_index) (v)
        let mutable matrix: int array array = g._adj_matrix
        __ret <- (_idx (_idx matrix (i)) (j)) = 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec clear_graph (g: Graph) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        g._vertex_to_index <- _dictCreate []
        g._adj_matrix <- [||]
        __ret
    with
        | Return -> __ret
let mutable g: Graph = make_graph (unbox<int array> [|1; 2; 3|]) ([|[|1; 2|]; [|2; 3|]|]) (false)
printfn "%s" (_str (g._adj_matrix))
printfn "%s" (_str (contains_edge (g) (1) (2)))
printfn "%s" (_str (contains_edge (g) (2) (1)))
remove_edge (g) (1) (2)
printfn "%s" (_str (contains_edge (g) (1) (2)))
remove_vertex (g) (2)
printfn "%s" (_str (g._adj_matrix))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
