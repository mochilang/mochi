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
type ExpandResult = {
    queue: int array
    head: int
    parents: System.Collections.Generic.IDictionary<int, int>
    visited: System.Collections.Generic.IDictionary<int, bool>
    intersection: int
    found: bool
}
type SearchResult = {
    path: int array
    ok: bool
}
open System.Collections.Generic

let rec expand_search (graph: System.Collections.Generic.IDictionary<int, int array>) (queue: int array) (head: int) (parents: System.Collections.Generic.IDictionary<int, int>) (visited: System.Collections.Generic.IDictionary<int, bool>) (opposite_visited: System.Collections.Generic.IDictionary<int, bool>) =
    let mutable __ret : ExpandResult = Unchecked.defaultof<ExpandResult>
    let mutable graph = graph
    let mutable queue = queue
    let mutable head = head
    let mutable parents = parents
    let mutable visited = visited
    let mutable opposite_visited = opposite_visited
    try
        if head >= (Seq.length (queue)) then
            __ret <- { queue = queue; head = head; parents = parents; visited = visited; intersection = 0 - 1; found = false }
            raise Return
        let current: int = _idx queue (head)
        head <- head + 1
        let neighbors: int array = _dictGet graph (current)
        let mutable q: int array = queue
        let mutable p: System.Collections.Generic.IDictionary<int, int> = parents
        let mutable v: System.Collections.Generic.IDictionary<int, bool> = visited
        let mutable i: int = 0
        try
            while i < (Seq.length (neighbors)) do
                try
                    let neighbor: int = _idx neighbors (i)
                    if _dictGet v (neighbor) then
                        i <- i + 1
                        raise Continue
                    v.[neighbor] <- true
                    p.[neighbor] <- current
                    q <- Array.append q [|neighbor|]
                    if _dictGet opposite_visited (neighbor) then
                        __ret <- { queue = q; head = head; parents = p; visited = v; intersection = neighbor; found = true }
                        raise Return
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { queue = q; head = head; parents = p; visited = v; intersection = 0 - 1; found = false }
        raise Return
        __ret
    with
        | Return -> __ret
and construct_path (current: int) (parents: System.Collections.Generic.IDictionary<int, int>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable current = current
    let mutable parents = parents
    try
        let mutable path: int array = [||]
        let mutable node: int = current
        while node <> (0 - 1) do
            path <- Array.append path [|node|]
            node <- _dictGet parents (node)
        __ret <- path
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_list (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = Seq.length (xs)
        while i > 0 do
            i <- i - 1
            res <- Array.append res [|(_idx xs (i))|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bidirectional_search (g: System.Collections.Generic.IDictionary<int, int array>) (start: int) (goal: int) =
    let mutable __ret : SearchResult = Unchecked.defaultof<SearchResult>
    let mutable g = g
    let mutable start = start
    let mutable goal = goal
    try
        if start = goal then
            __ret <- { path = [|start|]; ok = true }
            raise Return
        let mutable forward_parents: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        forward_parents.[start] <- 0 - 1
        let mutable backward_parents: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        backward_parents.[goal] <- 0 - 1
        let mutable forward_visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        forward_visited.[start] <- true
        let mutable backward_visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        backward_visited.[goal] <- true
        let mutable forward_queue: int array = [|start|]
        let mutable backward_queue: int array = [|goal|]
        let mutable forward_head: int = 0
        let mutable backward_head: int = 0
        let mutable intersection: int = 0 - 1
        try
            while ((forward_head < (Seq.length (forward_queue))) && (backward_head < (Seq.length (backward_queue)))) && (intersection = (0 - 1)) do
                try
                    let mutable res: ExpandResult = expand_search (g) (forward_queue) (forward_head) (forward_parents) (forward_visited) (backward_visited)
                    forward_queue <- res.queue
                    forward_head <- res.head
                    forward_parents <- res.parents
                    forward_visited <- res.visited
                    if res.found then
                        intersection <- res.intersection
                        raise Break
                    res <- expand_search (g) (backward_queue) (backward_head) (backward_parents) (backward_visited) (forward_visited)
                    backward_queue <- res.queue
                    backward_head <- res.head
                    backward_parents <- res.parents
                    backward_visited <- res.visited
                    if res.found then
                        intersection <- res.intersection
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if intersection = (0 - 1) then
            __ret <- { path = [||]; ok = false }
            raise Return
        let mutable forward_path: int array = construct_path (intersection) (forward_parents)
        forward_path <- reverse_list (forward_path)
        let mutable back_start: int = _dictGet backward_parents (intersection)
        let mutable backward_path: int array = construct_path (back_start) (backward_parents)
        let mutable result: int array = forward_path
        let mutable j: int = 0
        while j < (Seq.length (backward_path)) do
            result <- Array.append result [|(_idx backward_path (j))|]
            j <- j + 1
        __ret <- { path = result; ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and is_edge (g: System.Collections.Generic.IDictionary<int, int array>) (u: int) (v: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        let neighbors: int array = _dictGet g (u)
        let mutable i: int = 0
        while i < (Seq.length (neighbors)) do
            if (_idx neighbors (i)) = v then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and path_exists (g: System.Collections.Generic.IDictionary<int, int array>) (path: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable g = g
    let mutable path = path
    try
        if (Seq.length (path)) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while (i + 1) < (Seq.length (path)) do
            if not (is_edge (g) (_idx path (i)) (_idx path (i + 1))) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and print_path (g: System.Collections.Generic.IDictionary<int, int array>) (s: int) (t: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable s = s
    let mutable t = t
    try
        let mutable res: SearchResult = bidirectional_search (g) (s) (t)
        if (res.ok) && (path_exists (g) (res.path)) then
            printfn "%s" ((((("Path from " + (_str (s))) + " to ") + (_str (t))) + ": ") + (_str (res.path)))
        else
            printfn "%s" (((("Path from " + (_str (s))) + " to ") + (_str (t))) + ": None")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 2|]); (1, [|0; 3; 4|]); (2, [|0; 5; 6|]); (3, [|1; 7|]); (4, [|1; 8|]); (5, [|2; 9|]); (6, [|2; 10|]); (7, [|3; 11|]); (8, [|4; 11|]); (9, [|5; 11|]); (10, [|6; 11|]); (11, [|7; 8; 9; 10|])]
        print_path (graph) (0) (11)
        print_path (graph) (5) (5)
        let disconnected: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 2|]); (1, [|0|]); (2, [|0|]); (3, [|4|]); (4, [|3|])]
        print_path (disconnected) (0) (3)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
