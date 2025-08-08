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
type CheckResult = {
    mutable _status: int
    mutable _odd_node: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec make_matrix (n: int) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable n = n
    try
        let mutable matrix: bool array array = [||]
        let mutable i: int = 0
        while i <= n do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j <= n do
                row <- Array.append row [|false|]
                j <- j + 1
            matrix <- Array.append matrix [|row|]
            i <- i + 1
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec dfs (u: int) (graph: System.Collections.Generic.IDictionary<int, int array>) (visited_edge: bool array array) (path: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable u = u
    let mutable graph = graph
    let mutable visited_edge = visited_edge
    let mutable path = path
    try
        path <- Array.append path [|u|]
        if graph.ContainsKey(u) then
            let neighbors: int array = _dictGet graph (u)
            let mutable i: int = 0
            while i < (Seq.length (neighbors)) do
                let v: int = _idx neighbors (i)
                if (_idx (_idx visited_edge (u)) (v)) = false then
                    visited_edge.[u].[v] <- true
                    visited_edge.[v].[u] <- true
                    path <- dfs (v) (graph) (visited_edge) (path)
                i <- i + 1
        __ret <- path
        raise Return
        __ret
    with
        | Return -> __ret
let rec check_circuit_or_path (graph: System.Collections.Generic.IDictionary<int, int array>) (max_node: int) =
    let mutable __ret : CheckResult = Unchecked.defaultof<CheckResult>
    let mutable graph = graph
    let mutable max_node = max_node
    try
        let mutable odd_degree_nodes: int = 0
        let mutable _odd_node: int = -1
        let mutable i: int = 0
        while i < max_node do
            if graph.ContainsKey(i) then
                if ((((Seq.length (_dictGet graph (i))) % 2 + 2) % 2)) = 1 then
                    odd_degree_nodes <- odd_degree_nodes + 1
                    _odd_node <- i
            i <- i + 1
        if odd_degree_nodes = 0 then
            __ret <- { _status = 1; _odd_node = _odd_node }
            raise Return
        if odd_degree_nodes = 2 then
            __ret <- { _status = 2; _odd_node = _odd_node }
            raise Return
        __ret <- { _status = 3; _odd_node = _odd_node }
        raise Return
        __ret
    with
        | Return -> __ret
let rec check_euler (graph: System.Collections.Generic.IDictionary<int, int array>) (max_node: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable graph = graph
    let mutable max_node = max_node
    try
        let mutable visited_edge: bool array array = make_matrix (max_node)
        let res: CheckResult = check_circuit_or_path (graph) (max_node)
        if (res._status) = 3 then
            printfn "%s" ("graph is not Eulerian")
            printfn "%s" ("no path")
            __ret <- ()
            raise Return
        let mutable start_node: int = 1
        if (res._status) = 2 then
            start_node <- res._odd_node
            printfn "%s" ("graph has a Euler path")
        if (res._status) = 1 then
            printfn "%s" ("graph has a Euler cycle")
        let mutable path: int array = dfs (start_node) (graph) (visited_edge) (Array.empty<int>)
        printfn "%s" (_str (path))
        __ret
    with
        | Return -> __ret
let g1: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3; 4|]); (2, [|1; 3|]); (3, [|1; 2|]); (4, [|1; 5|]); (5, [|4|])]
let g2: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3; 4; 5|]); (2, [|1; 3|]); (3, [|1; 2|]); (4, [|1; 5|]); (5, [|1; 4|])]
let g3: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3; 4|]); (2, [|1; 3; 4|]); (3, [|1; 2|]); (4, [|1; 2; 5|]); (5, [|4|])]
let g4: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|1; 3|]); (3, [|1; 2|])]
let g5: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, Array.empty<int>); (2, Array.empty<int>)]
let max_node: int = 10
check_euler (g1) (max_node)
check_euler (g2) (max_node)
check_euler (g3) (max_node)
check_euler (g4) (max_node)
check_euler (g5) (max_node)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
