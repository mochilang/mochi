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
type Pos = {
    mutable _y: int
    mutable _x: int
}
type Node = {
    mutable _pos_x: int
    mutable _pos_y: int
    mutable _goal_x: int
    mutable _goal_y: int
    mutable _g_cost: int
    mutable _f_cost: int
    mutable _path: Pos array
}
let rec abs (_x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _x = _x
    try
        __ret <- if _x < 0 then (0 - _x) else _x
        raise Return
        __ret
    with
        | Return -> __ret
and manhattan (x1: int) (y1: int) (x2: int) (y2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable x2 = x2
    let mutable y2 = y2
    try
        __ret <- (abs (x1 - x2)) + (abs (y1 - y2))
        raise Return
        __ret
    with
        | Return -> __ret
and clone_path (p: Pos array) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable p = p
    try
        let mutable res: Pos array = [||]
        let mutable i: int = 0
        while i < (Seq.length (p)) do
            res <- Array.append res [|(_idx p (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and make_node (_pos_x: int) (_pos_y: int) (_goal_x: int) (_goal_y: int) (_g_cost: int) (_path: Pos array) =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    let mutable _pos_x = _pos_x
    let mutable _pos_y = _pos_y
    let mutable _goal_x = _goal_x
    let mutable _goal_y = _goal_y
    let mutable _g_cost = _g_cost
    let mutable _path = _path
    try
        let f: int = manhattan (_pos_x) (_pos_y) (_goal_x) (_goal_y)
        __ret <- { _pos_x = _pos_x; _pos_y = _pos_y; _goal_x = _goal_x; _goal_y = _goal_y; _g_cost = _g_cost; _f_cost = f; _path = _path }
        raise Return
        __ret
    with
        | Return -> __ret
let delta: Pos array = [|{ _y = -1; _x = 0 }; { _y = 0; _x = -1 }; { _y = 1; _x = 0 }; { _y = 0; _x = 1 }|]
let rec node_equal (a: Node) (b: Node) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- ((a._pos_x) = (b._pos_x)) && ((a._pos_y) = (b._pos_y))
        raise Return
        __ret
    with
        | Return -> __ret
and contains (nodes: Node array) (node: Node) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nodes = nodes
    let mutable node = node
    try
        let mutable i: int = 0
        while i < (Seq.length (nodes)) do
            if node_equal (_idx nodes (i)) (node) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and sort_nodes (nodes: Node array) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable nodes = nodes
    try
        let mutable arr: Node array = nodes
        let mutable i: int = 1
        try
            while i < (Seq.length (arr)) do
                try
                    let key_node: Node = _idx arr (i)
                    let mutable j: int = i - 1
                    try
                        while j >= 0 do
                            try
                                let temp: Node = _idx arr (j)
                                if (temp._f_cost) > (key_node._f_cost) then
                                    arr.[j + 1] <- temp
                                    j <- j - 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    arr.[j + 1] <- key_node
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and get_successors (grid: int array array) (parent: Node) (target: Pos) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable grid = grid
    let mutable parent = parent
    let mutable target = target
    try
        let mutable res: Node array = [||]
        let mutable i: int = 0
        while i < (Seq.length (delta)) do
            let d: Pos = _idx delta (i)
            let _pos_x: int = (parent._pos_x) + (d._x)
            let _pos_y: int = (parent._pos_y) + (d._y)
            if ((((_pos_x >= 0) && (_pos_x < (Seq.length (_idx grid (0))))) && (_pos_y >= 0)) && (_pos_y < (Seq.length (grid)))) && ((_idx (_idx grid (_pos_y)) (_pos_x)) = 0) then
                let mutable new_path: Pos array = clone_path (parent._path)
                new_path <- Array.append new_path [|{ _y = _pos_y; _x = _pos_x }|]
                res <- Array.append res [|(make_node (_pos_x) (_pos_y) (target._x) (target._y) ((parent._g_cost) + 1) (new_path))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and greedy_best_first (grid: int array array) (init: Pos) (goal: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable grid = grid
    let mutable init = init
    let mutable goal = goal
    try
        let start_path: Pos array = [|init|]
        let start: Node = make_node (init._x) (init._y) (goal._x) (goal._y) (0) (start_path)
        let mutable open_nodes: Node array = [|start|]
        let mutable closed_nodes: Node array = [||]
        while (Seq.length (open_nodes)) > 0 do
            open_nodes <- sort_nodes (open_nodes)
            let current: Node = _idx open_nodes (0)
            let mutable new_open: Node array = [||]
            let mutable idx: int = 1
            while idx < (Seq.length (open_nodes)) do
                new_open <- Array.append new_open [|(_idx open_nodes (idx))|]
                idx <- idx + 1
            open_nodes <- new_open
            if ((current._pos_x) = (goal._x)) && ((current._pos_y) = (goal._y)) then
                __ret <- current._path
                raise Return
            closed_nodes <- Array.append closed_nodes [|current|]
            let successors: Node array = get_successors (grid) (current) (goal)
            let mutable i: int = 0
            while i < (Seq.length (successors)) do
                let child: Node = _idx successors (i)
                if (not (contains (closed_nodes) (child))) && (not (contains (open_nodes) (child))) then
                    open_nodes <- Array.append open_nodes [|child|]
                i <- i + 1
        let r: Pos array = [|init|]
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let TEST_GRIDS: int array array array = [|[|[|0; 0; 0; 0; 0; 0; 0|]; [|0; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 1; 0; 0; 0; 0|]; [|1; 0; 1; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 1; 0; 0|]|]; [|[|0; 0; 0; 1; 1; 0; 0|]; [|0; 0; 0; 0; 1; 0; 1|]; [|0; 0; 0; 1; 1; 0; 0|]; [|0; 1; 0; 0; 1; 0; 0|]; [|1; 0; 0; 1; 1; 0; 1|]; [|0; 0; 0; 0; 0; 0; 0|]|]; [|[|0; 0; 1; 0; 0|]; [|0; 1; 0; 0; 0|]; [|0; 0; 1; 0; 1|]; [|1; 0; 0; 1; 1|]; [|0; 0; 0; 0; 0|]|]|]
let rec print_grid (grid: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable grid = grid
    try
        let mutable i: int = 0
        while i < (Seq.length (grid)) do
            printfn "%s" (_str (_idx grid (i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable idx: int = 0
        while idx < (Seq.length (TEST_GRIDS)) do
            printfn "%s" (("==grid-" + (_str (idx + 1))) + "==")
            let mutable grid: int array array = _idx TEST_GRIDS (idx)
            let init: Pos = { _y = 0; _x = 0 }
            let goal: Pos = { _y = (Seq.length (grid)) - 1; _x = (Seq.length (_idx grid (0))) - 1 }
            print_grid (grid)
            printfn "%s" ("------")
            let _path: Pos array = greedy_best_first (grid) (init) (goal)
            let mutable j: int = 0
            while j < (Seq.length (_path)) do
                let p: Pos = _idx _path (j)
                grid.[p._y].[p._x] <- 2
                j <- j + 1
            print_grid (grid)
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
