// Generated 2025-08-08 11:10 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type Node = {
    mutable point: float array
    mutable _left: int
    mutable _right: int
}
type BuildResult = {
    mutable _index: int
    mutable _nodes: Node array
}
type SearchResult = {
    mutable point: float array
    mutable _dist: float
    mutable _visited: int
}
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and hypercube_points (num_points: int) (cube_size: float) (num_dimensions: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable num_points = num_points
    let mutable cube_size = cube_size
    let mutable num_dimensions = num_dimensions
    try
        let mutable pts: float array array = [||]
        let mutable i: int = 0
        while i < num_points do
            let mutable p: float array = [||]
            let mutable j: int = 0
            while j < num_dimensions do
                p <- Array.append p [|(cube_size * (random()))|]
                j <- j + 1
            pts <- Array.append pts [|p|]
            i <- i + 1
        __ret <- pts
        raise Return
        __ret
    with
        | Return -> __ret
and sort_points (points: float array array) (axis: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable points = points
    let mutable axis = axis
    try
        let n: int = Seq.length (points)
        let mutable i: int = 1
        while i < n do
            let mutable key: float array = _idx points (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx (_idx points (j)) (axis)) > (_idx key (axis))) do
                points.[j + 1] <- _idx points (j)
                j <- j - 1
            points.[j + 1] <- key
            i <- i + 1
        __ret <- points
        raise Return
        __ret
    with
        | Return -> __ret
and sublist (arr: float array array) (start: int) (``end``: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable arr = arr
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: float array array = [||]
        let mutable i: int = start
        while i < ``end`` do
            res <- Array.append res [|(_idx arr (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and shift_nodes (_nodes: Node array) (offset: int) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable _nodes = _nodes
    let mutable offset = offset
    try
        let mutable i: int = 0
        while i < (Seq.length (_nodes)) do
            if ((_idx _nodes (i))._left) <> (0 - 1) then
                _nodes.[i]._left <- ((_idx _nodes (i))._left) + offset
            if ((_idx _nodes (i))._right) <> (0 - 1) then
                _nodes.[i]._right <- ((_idx _nodes (i))._right) + offset
            i <- i + 1
        __ret <- _nodes
        raise Return
        __ret
    with
        | Return -> __ret
and build_kdtree (points: float array array) (depth: int) =
    let mutable __ret : BuildResult = Unchecked.defaultof<BuildResult>
    let mutable points = points
    let mutable depth = depth
    try
        if (Seq.length (points)) = 0 then
            __ret <- { _index = 0 - 1; _nodes = [||] }
            raise Return
        let k: int = Seq.length (_idx points (0))
        let axis: int = ((depth % k + k) % k)
        points <- sort_points (points) (axis)
        let median: int = _floordiv (Seq.length (points)) 2
        let left_points: float array array = sublist (points) (0) (median)
        let right_points: float array array = sublist (points) (median + 1) (Seq.length (points))
        let left_res: BuildResult = build_kdtree (left_points) (depth + 1)
        let right_res: BuildResult = build_kdtree (right_points) (depth + 1)
        let offset: int = (Seq.length (left_res._nodes)) + 1
        let shifted_right: Node array = shift_nodes (right_res._nodes) (offset)
        let mutable _nodes: Node array = left_res._nodes
        let left_index: int = left_res._index
        let right_index: int = if (right_res._index) = (0 - 1) then (0 - 1) else ((right_res._index) + offset)
        _nodes <- Array.append _nodes [|{ point = _idx points (median); _left = left_index; _right = right_index }|]
        _nodes <- unbox<Node array> (Array.append (_nodes) (shifted_right))
        let root_index: int = Seq.length (left_res._nodes)
        __ret <- { _index = root_index; _nodes = _nodes }
        raise Return
        __ret
    with
        | Return -> __ret
and square_distance (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let diff: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and nearest_neighbour_search (tree: Node array) (root: int) (query_point: float array) =
    let mutable __ret : SearchResult = Unchecked.defaultof<SearchResult>
    let mutable tree = tree
    let mutable root = root
    let mutable query_point = query_point
    try
        let mutable nearest_point: float array = [||]
        let mutable nearest_dist: float = 0.0
        let mutable _visited: int = 0
        let mutable i: int = 0
        while i < (Seq.length (tree)) do
            let node: Node = _idx tree (i)
            let _dist: float = square_distance (query_point) (node.point)
            _visited <- _visited + 1
            if (_visited = 1) || (_dist < nearest_dist) then
                nearest_point <- node.point
                nearest_dist <- _dist
            i <- i + 1
        __ret <- { point = nearest_point; _dist = nearest_dist; _visited = _visited }
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (arr: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            s <- s + (_str (_idx arr (i)))
            if i < ((Seq.length (arr)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let num_points: int = 5000
        let cube_size: float = 10.0
        let num_dimensions: int = 10
        let mutable pts: float array array = hypercube_points (num_points) (cube_size) (num_dimensions)
        let build: BuildResult = build_kdtree (pts) (0)
        let root: int = build._index
        let tree: Node array = build._nodes
        let mutable query: float array = [||]
        let mutable i: int = 0
        while i < num_dimensions do
            query <- Array.append query [|(random())|]
            i <- i + 1
        let mutable res: SearchResult = nearest_neighbour_search (tree) (root) (query)
        printfn "%s" ("Query point: " + (list_to_string (query)))
        printfn "%s" ("Nearest point: " + (list_to_string (res.point)))
        printfn "%s" ("Distance: " + (_str (res._dist)))
        printfn "%s" ("Nodes visited: " + (_str (res._visited)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
