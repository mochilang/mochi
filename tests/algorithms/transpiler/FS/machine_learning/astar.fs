// Generated 2025-08-08 17:07 +0700

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
type Point = {
    mutable _x: int
    mutable _y: int
}
type Node = {
    mutable _pos: Point
    mutable _parent: Point
    mutable _g: int
    mutable _h: int
    mutable _f: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_neighbours (p: Point) (x_limit: int) (y_limit: int) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable p = p
    let mutable x_limit = x_limit
    let mutable y_limit = y_limit
    try
        let deltas: Point array = unbox<Point array> [|{ _x = 0 - 1; _y = 0 - 1 }; { _x = 0 - 1; _y = 0 }; { _x = 0 - 1; _y = 1 }; { _x = 0; _y = 0 - 1 }; { _x = 0; _y = 1 }; { _x = 1; _y = 0 - 1 }; { _x = 1; _y = 0 }; { _x = 1; _y = 1 }|]
        let mutable neighbours: Point array = Array.empty<Point>
        for d in deltas do
            let nx: int = (p._x) + (d._x)
            let ny: int = (p._y) + (d._y)
            if (((0 <= nx) && (nx < x_limit)) && (0 <= ny)) && (ny < y_limit) then
                neighbours <- Array.append neighbours [|{ _x = nx; _y = ny }|]
        __ret <- neighbours
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (nodes: Node array) (p: Point) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nodes = nodes
    let mutable p = p
    try
        for n in nodes do
            if (((n._pos)._x) = (p._x)) && (((n._pos)._y) = (p._y)) then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_node (nodes: Node array) (p: Point) =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    let mutable nodes = nodes
    let mutable p = p
    try
        for n in nodes do
            if (((n._pos)._x) = (p._x)) && (((n._pos)._y) = (p._y)) then
                __ret <- n
                raise Return
        __ret <- { _pos = p; _parent = { _x = 0 - 1; _y = 0 - 1 }; _g = 0; _h = 0; _f = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec astar (x_limit: int) (y_limit: int) (start: Point) (goal: Point) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable x_limit = x_limit
    let mutable y_limit = y_limit
    let mutable start = start
    let mutable goal = goal
    try
        let mutable ``open``: Node array = Array.empty<Node>
        let mutable closed: Node array = Array.empty<Node>
        ``open`` <- Array.append ``open`` [|{ _pos = start; _parent = { _x = 0 - 1; _y = 0 - 1 }; _g = 0; _h = 0; _f = 0 }|]
        let mutable current: Node = _idx ``open`` (0)
        try
            while (Seq.length (``open``)) > 0 do
                try
                    let mutable min_index: int = 0
                    let mutable i: int = 1
                    while i < (Seq.length (``open``)) do
                        if ((_idx ``open`` (i))._f) < ((_idx ``open`` (min_index))._f) then
                            min_index <- i
                        i <- i + 1
                    current <- _idx ``open`` (min_index)
                    let mutable new_open: Node array = Array.empty<Node>
                    let mutable j: int = 0
                    while j < (Seq.length (``open``)) do
                        if j <> min_index then
                            new_open <- Array.append new_open [|(_idx ``open`` (j))|]
                        j <- j + 1
                    ``open`` <- new_open
                    closed <- Array.append closed [|current|]
                    if (((current._pos)._x) = (goal._x)) && (((current._pos)._y) = (goal._y)) then
                        raise Break
                    let mutable neighbours: Point array = get_neighbours (current._pos) (x_limit) (y_limit)
                    try
                        for np in neighbours do
                            try
                                if contains (closed) (np) then
                                    raise Continue
                                let _g: int = (current._g) + 1
                                let dx: int = (goal._x) - (np._x)
                                let dy: int = (goal._y) - (np._y)
                                let _h: int = (dx * dx) + (dy * dy)
                                let _f: int = _g + _h
                                let mutable skip: bool = false
                                for node in ``open`` do
                                    if ((((node._pos)._x) = (np._x)) && (((node._pos)._y) = (np._y))) && ((node._f) < _f) then
                                        skip <- true
                                if skip then
                                    raise Continue
                                ``open`` <- Array.append ``open`` [|{ _pos = np; _parent = current._pos; _g = _g; _h = _h; _f = _f }|]
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
        let mutable path: Point array = Array.empty<Point>
        path <- Array.append path [|(current._pos)|]
        while not ((((current._parent)._x) = (0 - 1)) && (((current._parent)._y) = (0 - 1))) do
            current <- get_node (closed) (current._parent)
            path <- Array.append path [|(current._pos)|]
        let mutable rev: Point array = Array.empty<Point>
        let mutable k: int = (Seq.length (path)) - 1
        while k >= 0 do
            rev <- Array.append rev [|(_idx path (k))|]
            k <- k - 1
        __ret <- rev
        raise Return
        __ret
    with
        | Return -> __ret
let rec create_world (x_limit: int) (y_limit: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable x_limit = x_limit
    let mutable y_limit = y_limit
    try
        let mutable world: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < x_limit do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < y_limit do
                row <- Array.append row [|0|]
                j <- j + 1
            world <- Array.append world [|row|]
            i <- i + 1
        __ret <- world
        raise Return
        __ret
    with
        | Return -> __ret
let rec mark_path (world: int array array) (path: Point array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable world = world
    let mutable path = path
    try
        for p in path do
            world.[p._x].[p._y] <- 1
        __ret
    with
        | Return -> __ret
let rec print_world (world: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable world = world
    try
        for row in world do
            printfn "%s" (_str (row))
        __ret
    with
        | Return -> __ret
let world_x: int = 5
let world_y: int = 5
let start: Point = { _x = 0; _y = 0 }
let goal: Point = { _x = 4; _y = 4 }
let mutable path: Point array = astar (world_x) (world_y) (start) (goal)
printfn "%s" (((((((("path from (" + (_str (start._x))) + ", ") + (_str (start._y))) + ") to (") + (_str (goal._x))) + ", ") + (_str (goal._y))) + ")")
let mutable world: int array array = create_world (world_x) (world_y)
mark_path (world) (path)
print_world (world)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
