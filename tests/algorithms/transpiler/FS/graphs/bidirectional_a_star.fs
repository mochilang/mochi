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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Pos = {
    y: int
    x: int
}
type Node = {
    pos: Pos
    g_cost: int
    h_cost: float
    f_cost: float
    path: Pos array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let HEURISTIC: int = 0
let grid: int array array = [|[|0; 0; 0; 0; 0; 0; 0|]; [|0; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 1; 0; 0; 0; 0|]; [|1; 0; 1; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 1; 0; 0|]|]
let delta: int array array = [|[|-1; 0|]; [|0; -1|]; [|1; 0|]; [|0; 1|]|]
let rec abs (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec heuristic (a: Pos) (b: Pos) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let dy: int = (a.y) - (b.y)
        let dx: int = (a.x) - (b.x)
        if HEURISTIC = 1 then
            __ret <- float ((abs (dy)) + (abs (dx)))
            raise Return
        let dyf: float = float dy
        let dxf: float = float dx
        __ret <- sqrtApprox ((dyf * dyf) + (dxf * dxf))
        raise Return
        __ret
    with
        | Return -> __ret
let rec pos_equal (a: Pos) (b: Pos) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- ((a.y) = (b.y)) && ((a.x) = (b.x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_pos (lst: Pos array) (p: Pos) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable lst = lst
    let mutable p = p
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if pos_equal (_idx lst (i)) (p) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec open_index_of_pos (``open``: Node array) (p: Pos) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``open`` = ``open``
    let mutable p = p
    try
        let mutable i: int = 0
        while i < (Seq.length (``open``)) do
            if pos_equal ((_idx ``open`` (i)).pos) (p) then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_node_at (nodes: Node array) (idx: int) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        let mutable res: Node array = [||]
        let mutable i: int = 0
        while i < (Seq.length (nodes)) do
            if i <> idx then
                res <- Array.append res [|(_idx nodes (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec append_pos_list (path: Pos array) (p: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable path = path
    let mutable p = p
    try
        let mutable res: Pos array = [||]
        let mutable i: int = 0
        while i < (Seq.length (path)) do
            res <- Array.append res [|(_idx path (i))|]
            i <- i + 1
        res <- Array.append res [|p|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_pos_list (lst: Pos array) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable lst = lst
    try
        let mutable res: Pos array = [||]
        let mutable i: int = (Seq.length (lst)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx lst (i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec concat_pos_lists (a: Pos array) (b: Pos array) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: Pos array = [||]
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|(_idx a (i))|]
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (b)) do
            res <- Array.append res [|(_idx b (j))|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_successors (p: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable p = p
    try
        let mutable res: Pos array = [||]
        let mutable i: int = 0
        while i < (Seq.length (delta)) do
            let nx: int = (p.x) + (_idx (_idx delta (i)) (1))
            let ny: int = (p.y) + (_idx (_idx delta (i)) (0))
            if (((nx >= 0) && (ny >= 0)) && (nx < (Seq.length (_idx grid (0))))) && (ny < (Seq.length (grid))) then
                if (_idx (_idx grid (ny)) (nx)) = 0 then
                    res <- Array.append res [|{ y = ny; x = nx }|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_lowest_f (``open``: Node array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``open`` = ``open``
    try
        let mutable best: int = 0
        let mutable i: int = 1
        while i < (Seq.length (``open``)) do
            if ((_idx ``open`` (i)).f_cost) < ((_idx ``open`` (best)).f_cost) then
                best <- i
            i <- i + 1
        __ret <- best
        raise Return
        __ret
    with
        | Return -> __ret
let rec astar (start: Pos) (goal: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable start = start
    let mutable goal = goal
    try
        let h0: float = heuristic (start) (goal)
        let mutable ``open``: Node array = [|{ pos = start; g_cost = 0; h_cost = h0; f_cost = h0; path = [|start|] }|]
        let mutable closed: Pos array = [||]
        try
            while (Seq.length (``open``)) > 0 do
                try
                    let idx: int = find_lowest_f (``open``)
                    let current: Node = _idx ``open`` (idx)
                    ``open`` <- remove_node_at (``open``) (idx)
                    if pos_equal (current.pos) (goal) then
                        __ret <- current.path
                        raise Return
                    closed <- Array.append closed [|(current.pos)|]
                    let succ: Pos array = get_successors (current.pos)
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (succ)) do
                            try
                                let pos: Pos = _idx succ (i)
                                if contains_pos (closed) (pos) then
                                    i <- i + 1
                                    raise Continue
                                let tentative_g: int = (current.g_cost) + 1
                                let idx_open: int = open_index_of_pos (``open``) (pos)
                                if (idx_open = (0 - 1)) || (tentative_g < ((_idx ``open`` (idx_open)).g_cost)) then
                                    let new_path: Pos array = append_pos_list (current.path) (pos)
                                    let h: float = heuristic (pos) (goal)
                                    let f: float = (float tentative_g) + h
                                    if idx_open <> (0 - 1) then
                                        ``open`` <- remove_node_at (``open``) (idx_open)
                                    ``open`` <- Array.append ``open`` [|{ pos = pos; g_cost = tentative_g; h_cost = h; f_cost = f; path = new_path }|]
                                i <- i + 1
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
        __ret <- unbox<Pos array> [|start|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec combine_paths (fwd: Node) (bwd: Node) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable fwd = fwd
    let mutable bwd = bwd
    try
        let mutable bwd_copy: Pos array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (bwd.path)) - 1) do
            bwd_copy <- Array.append bwd_copy [|(_idx (bwd.path) (i))|]
            i <- i + 1
        bwd_copy <- reverse_pos_list (bwd_copy)
        __ret <- concat_pos_lists (fwd.path) (bwd_copy)
        raise Return
        __ret
    with
        | Return -> __ret
let rec bidirectional_astar (start: Pos) (goal: Pos) =
    let mutable __ret : Pos array = Unchecked.defaultof<Pos array>
    let mutable start = start
    let mutable goal = goal
    try
        let hf: float = heuristic (start) (goal)
        let hb: float = heuristic (goal) (start)
        let mutable open_f: Node array = [|{ pos = start; g_cost = 0; h_cost = hf; f_cost = hf; path = [|start|] }|]
        let mutable open_b: Node array = [|{ pos = goal; g_cost = 0; h_cost = hb; f_cost = hb; path = [|goal|] }|]
        let mutable closed_f: Pos array = [||]
        let mutable closed_b: Pos array = [||]
        try
            while ((Seq.length (open_f)) > 0) && ((Seq.length (open_b)) > 0) do
                try
                    let idx_f: int = find_lowest_f (open_f)
                    let current_f: Node = _idx open_f (idx_f)
                    open_f <- remove_node_at (open_f) (idx_f)
                    let idx_b: int = find_lowest_f (open_b)
                    let current_b: Node = _idx open_b (idx_b)
                    open_b <- remove_node_at (open_b) (idx_b)
                    if pos_equal (current_f.pos) (current_b.pos) then
                        __ret <- combine_paths (current_f) (current_b)
                        raise Return
                    closed_f <- Array.append closed_f [|(current_f.pos)|]
                    closed_b <- Array.append closed_b [|(current_b.pos)|]
                    let succ_f: Pos array = get_successors (current_f.pos)
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (succ_f)) do
                            try
                                let pos: Pos = _idx succ_f (i)
                                if contains_pos (closed_f) (pos) then
                                    i <- i + 1
                                    raise Continue
                                let tentative_g: int = (current_f.g_cost) + 1
                                let h: float = heuristic (pos) (current_b.pos)
                                let f: float = (float tentative_g) + h
                                let idx_open: int = open_index_of_pos (open_f) (pos)
                                if (idx_open = (0 - 1)) || (tentative_g < ((_idx open_f (idx_open)).g_cost)) then
                                    let new_path: Pos array = append_pos_list (current_f.path) (pos)
                                    if idx_open <> (0 - 1) then
                                        open_f <- remove_node_at (open_f) (idx_open)
                                    open_f <- Array.append open_f [|{ pos = pos; g_cost = tentative_g; h_cost = h; f_cost = f; path = new_path }|]
                                i <- i + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    let succ_b: Pos array = get_successors (current_b.pos)
                    i <- 0
                    try
                        while i < (Seq.length (succ_b)) do
                            try
                                let pos: Pos = _idx succ_b (i)
                                if contains_pos (closed_b) (pos) then
                                    i <- i + 1
                                    raise Continue
                                let tentative_g: int = (current_b.g_cost) + 1
                                let h: float = heuristic (pos) (current_f.pos)
                                let f: float = (float tentative_g) + h
                                let idx_open: int = open_index_of_pos (open_b) (pos)
                                if (idx_open = (0 - 1)) || (tentative_g < ((_idx open_b (idx_open)).g_cost)) then
                                    let new_path: Pos array = append_pos_list (current_b.path) (pos)
                                    if idx_open <> (0 - 1) then
                                        open_b <- remove_node_at (open_b) (idx_open)
                                    open_b <- Array.append open_b [|{ pos = pos; g_cost = tentative_g; h_cost = h; f_cost = f; path = new_path }|]
                                i <- i + 1
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
        __ret <- unbox<Pos array> [|start|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec path_to_string (path: Pos array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable path = path
    try
        if (Seq.length (path)) = 0 then
            __ret <- "[]"
            raise Return
        let mutable s: string = ((("[(" + (_str ((_idx path (0)).y))) + ", ") + (_str ((_idx path (0)).x))) + ")"
        let mutable i: int = 1
        while i < (Seq.length (path)) do
            s <- ((((s + ", (") + (_str ((_idx path (i)).y))) + ", ") + (_str ((_idx path (i)).x))) + ")"
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let start: Pos = { y = 0; x = 0 }
let goal: Pos = { y = (Seq.length (grid)) - 1; x = (Seq.length (_idx grid (0))) - 1 }
let path1: Pos array = astar (start) (goal)
printfn "%s" (path_to_string (path1))
let path2: Pos array = bidirectional_astar (start) (goal)
printfn "%s" (path_to_string (path2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
