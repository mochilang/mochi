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
type Graph = {
    vertex: System.Collections.Generic.IDictionary<int, int array>
    size: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec add_edge (g: Graph) (from_vertex: int) (to_vertex: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    let mutable from_vertex = from_vertex
    let mutable to_vertex = to_vertex
    try
        let mutable v: System.Collections.Generic.IDictionary<int, int array> = g.vertex
        if v.ContainsKey(from_vertex) then
            let mutable lst: int array = _dictGet v (from_vertex)
            lst <- Array.append lst [|to_vertex|]
            v.[from_vertex] <- lst
        else
            v.[from_vertex] <- [|to_vertex|]
        g <- { g with vertex = v }
        if (from_vertex + 1) > (g.size) then
            g <- { g with size = from_vertex + 1 }
        if (to_vertex + 1) > (g.size) then
            g <- { g with size = to_vertex + 1 }
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (lst: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            res <- res + (_str (_idx lst (i)))
            if i < ((Seq.length (lst)) - 1) then
                res <- res + " "
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_arrow (lst: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            res <- res + (_str (_idx lst (i)))
            if i < ((Seq.length (lst)) - 1) then
                res <- res + " -> "
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_graph (g: Graph) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        printfn "%s" (_str (g.vertex))
        let mutable i: int = 0
        while i < (g.size) do
            let mutable edges: int array = [||]
            if (g.vertex).ContainsKey(i) then
                edges <- _dictGet (g.vertex) (i)
            let line: string = ((_str (i)) + "  ->  ") + (list_to_arrow (edges))
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec dfs_recursive (g: Graph) (start_vertex: int) (visited: bool array) (order: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable start_vertex = start_vertex
    let mutable visited = visited
    let mutable order = order
    try
        visited.[start_vertex] <- true
        order <- Array.append order [|start_vertex|]
        if (g.vertex).ContainsKey(start_vertex) then
            let mutable neighbors: int array = _dictGet (g.vertex) (start_vertex)
            let mutable i: int = 0
            while i < (Seq.length (neighbors)) do
                let nb: int = _idx neighbors (i)
                if not (_idx visited (nb)) then
                    order <- dfs_recursive (g) (nb) (visited) (order)
                i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
let rec dfs (g: Graph) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    try
        let n: int = g.size
        let mutable visited: bool array = [||]
        let mutable i: int = 0
        while i < n do
            visited <- Array.append visited [|false|]
            i <- i + 1
        let mutable order: int array = [||]
        i <- 0
        while i < n do
            if not (_idx visited (i)) then
                order <- dfs_recursive (g) (i) (visited) (order)
            i <- i + 1
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
let mutable g: Graph = { vertex = _dictCreate []; size = 0 }
g <- add_edge (g) (0) (1)
g <- add_edge (g) (0) (2)
g <- add_edge (g) (1) (2)
g <- add_edge (g) (2) (0)
g <- add_edge (g) (2) (3)
g <- add_edge (g) (3) (3)
print_graph (g)
printfn "%s" ("DFS:")
printfn "%s" (list_to_string (dfs (g)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
