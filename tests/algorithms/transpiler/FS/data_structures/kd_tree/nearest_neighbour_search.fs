// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type KDNode = {
    point: float array
    left: int
    right: int
}
type SearchResult = {
    point: float array
    distance: float
    nodes_visited: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec square_distance (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int = 0
        let mutable total: float = 0.0
        while i < (Seq.length (a)) do
            let diff: float = (_idx a (i)) - (_idx b (i))
            total <- total + (diff * diff)
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec search (nodes: KDNode array) (index: int) (query_point: float array) (depth: int) (best: SearchResult) =
    let mutable __ret : SearchResult = Unchecked.defaultof<SearchResult>
    let mutable nodes = nodes
    let mutable index = index
    let mutable query_point = query_point
    let mutable depth = depth
    let mutable best = best
    try
        if index = (-1) then
            __ret <- best
            raise Return
        let mutable result: SearchResult = best
        result <- { result with nodes_visited = (result.nodes_visited) + 1 }
        let node: KDNode = _idx nodes (index)
        let current_point: float array = node.point
        let current_dist: float = square_distance (query_point) (current_point)
        if ((Seq.length (result.point)) = 0) || (current_dist < (result.distance)) then
            result <- { result with point = current_point }
            result <- { result with distance = current_dist }
        let k: int = Seq.length (query_point)
        let axis: int = ((depth % k + k) % k)
        let mutable nearer: int = node.left
        let mutable further: int = node.right
        if (_idx query_point (axis)) > (_idx current_point (axis)) then
            nearer <- node.right
            further <- node.left
        result <- search (nodes) (nearer) (query_point) (depth + 1) (result)
        let diff: float = (_idx query_point (axis)) - (_idx current_point (axis))
        if (diff * diff) < (result.distance) then
            result <- search (nodes) (further) (query_point) (depth + 1) (result)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec nearest_neighbour_search (nodes: KDNode array) (root: int) (query_point: float array) =
    let mutable __ret : SearchResult = Unchecked.defaultof<SearchResult>
    let mutable nodes = nodes
    let mutable root = root
    let mutable query_point = query_point
    try
        let initial: SearchResult = { point = [||]; distance = 1000000000000000000000000000000.0; nodes_visited = 0 }
        __ret <- search (nodes) (root) (query_point) (0) (initial)
        raise Return
        __ret
    with
        | Return -> __ret
let nodes: KDNode array = [|{ point = [|9.0; 1.0|]; left = 1; right = 4 }; { point = [|2.0; 7.0|]; left = 2; right = 3 }; { point = [|3.0; 6.0|]; left = -1; right = -1 }; { point = [|6.0; 12.0|]; left = -1; right = -1 }; { point = [|17.0; 15.0|]; left = 5; right = 6 }; { point = [|13.0; 15.0|]; left = -1; right = -1 }; { point = [|10.0; 19.0|]; left = -1; right = -1 }|]
let queries: float array array = [|[|9.0; 2.0|]; [|12.0; 15.0|]; [|1.0; 3.0|]|]
let mutable q: int = 0
while q < (Seq.length (queries)) do
    let res: SearchResult = nearest_neighbour_search (nodes) (0) (_idx queries (q))
    printfn "%s" ((((((_str (res.point)) + " ") + (_str (res.distance))) + " ") + (_str (res.nodes_visited))) + "\n")
    q <- q + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
