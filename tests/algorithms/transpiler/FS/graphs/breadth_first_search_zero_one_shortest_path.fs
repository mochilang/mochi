// Generated 2025-08-07 16:27 +0700

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
    destination_vertex: int
    weight: int
}
type AdjacencyList = {
    graph: Edge array array
    size: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_adjacency_list (size: int) =
    let mutable __ret : AdjacencyList = Unchecked.defaultof<AdjacencyList>
    let mutable size = size
    try
        let mutable g: Edge array array = [||]
        let mutable i: int = 0
        while i < size do
            g <- Array.append g [|[||]|]
            i <- i + 1
        __ret <- { graph = g; size = size }
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_edge (al: AdjacencyList) (from_vertex: int) (to_vertex: int) (weight: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable al = al
    let mutable from_vertex = from_vertex
    let mutable to_vertex = to_vertex
    let mutable weight = weight
    try
        if not ((weight = 0) || (weight = 1)) then
            failwith ("Edge weight must be either 0 or 1.")
        if (to_vertex < 0) || (to_vertex >= (al.size)) then
            failwith ("Vertex indexes must be in [0; size).")
        let mutable g: Edge array array = al.graph
        let edges: Edge array = _idx g (from_vertex)
        g.[from_vertex] <- Array.append edges [|{ destination_vertex = to_vertex; weight = weight }|]
        al <- { al with graph = g }
        __ret
    with
        | Return -> __ret
let rec push_front (q: int array) (v: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable q = q
    let mutable v = v
    try
        let mutable res: int array = [|v|]
        let mutable i: int = 0
        while i < (Seq.length (q)) do
            res <- Array.append res [|(_idx q (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec pop_front (q: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable q = q
    try
        let mutable res: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (q)) do
            res <- Array.append res [|(_idx q (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec front (q: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- _idx q (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_shortest_path (al: AdjacencyList) (start_vertex: int) (finish_vertex: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable al = al
    let mutable start_vertex = start_vertex
    let mutable finish_vertex = finish_vertex
    try
        let mutable queue: int array = [|start_vertex|]
        let mutable distances: int array = [||]
        let mutable i: int = 0
        while i < (al.size) do
            distances <- Array.append distances [|(-1)|]
            i <- i + 1
        distances.[start_vertex] <- 0
        try
            while (Seq.length (queue)) > 0 do
                try
                    let current_vertex: int = front (queue)
                    queue <- pop_front (queue)
                    let current_distance: int = _idx distances (current_vertex)
                    let mutable edges: Edge array = _idx (al.graph) (current_vertex)
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (edges)) do
                            try
                                let edge: Edge = _idx edges (j)
                                let new_distance: int = current_distance + (edge.weight)
                                let dest: int = edge.destination_vertex
                                let dest_distance: int = _idx distances (dest)
                                if (dest_distance >= 0) && (new_distance >= dest_distance) then
                                    j <- j + 1
                                    raise Continue
                                distances.[dest] <- new_distance
                                if (edge.weight) = 0 then
                                    queue <- push_front (queue) (dest)
                                else
                                    queue <- Array.append queue [|dest|]
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let result: int = _idx distances (finish_vertex)
        if result < 0 then
            failwith ("No path from start_vertex to finish_vertex.")
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let mutable g: AdjacencyList = new_adjacency_list (11)
add_edge (g) (0) (1) (0)
add_edge (g) (0) (3) (1)
add_edge (g) (1) (2) (0)
add_edge (g) (2) (3) (0)
add_edge (g) (4) (2) (1)
add_edge (g) (4) (5) (1)
add_edge (g) (4) (6) (1)
add_edge (g) (5) (9) (0)
add_edge (g) (6) (7) (1)
add_edge (g) (7) (8) (1)
add_edge (g) (8) (10) (1)
add_edge (g) (9) (7) (0)
add_edge (g) (9) (10) (1)
printfn "%s" (_str (get_shortest_path (g) (0) (3)))
printfn "%s" (_str (get_shortest_path (g) (4) (10)))
printfn "%s" (_str (get_shortest_path (g) (4) (8)))
printfn "%s" (_str (get_shortest_path (g) (0) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
