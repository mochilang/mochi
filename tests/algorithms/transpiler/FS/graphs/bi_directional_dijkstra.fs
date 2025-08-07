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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Edge = {
    ``to``: string
    cost: int
}
type QItem = {
    node: string
    cost: int
}
type PassResult = {
    queue: QItem array
    dist: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec get_min_index (q: QItem array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        let mutable idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (q)) do
            if ((_idx q (i)).cost) < ((_idx q (idx)).cost) then
                idx <- i
            i <- i + 1
        __ret <- idx
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_at (q: QItem array) (idx: int) =
    let mutable __ret : QItem array = Unchecked.defaultof<QItem array>
    let mutable q = q
    let mutable idx = idx
    try
        let mutable res: QItem array = [||]
        let mutable i: int = 0
        while i < (Seq.length (q)) do
            if i <> idx then
                res <- Array.append res [|(_idx q (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec pass_and_relaxation (graph: System.Collections.Generic.IDictionary<string, Edge array>) (v: string) (visited_forward: System.Collections.Generic.IDictionary<string, bool>) (visited_backward: System.Collections.Generic.IDictionary<string, bool>) (cst_fwd: System.Collections.Generic.IDictionary<string, int>) (cst_bwd: System.Collections.Generic.IDictionary<string, int>) (queue: QItem array) (parent: System.Collections.Generic.IDictionary<string, string>) (shortest_distance: int) =
    let mutable __ret : PassResult = Unchecked.defaultof<PassResult>
    let mutable graph = graph
    let mutable v = v
    let mutable visited_forward = visited_forward
    let mutable visited_backward = visited_backward
    let mutable cst_fwd = cst_fwd
    let mutable cst_bwd = cst_bwd
    let mutable queue = queue
    let mutable parent = parent
    let mutable shortest_distance = shortest_distance
    try
        let mutable q: QItem array = queue
        let mutable sd: int = shortest_distance
        try
            for e in graph.[(string (v))] do
                try
                    let nxt: string = e.``to``
                    let d: int = e.cost
                    if visited_forward.ContainsKey(nxt) then
                        raise Continue
                    let old_cost: int = if cst_fwd.ContainsKey(nxt) then (cst_fwd.[(string (nxt))]) else 2147483647
                    let new_cost: int = (cst_fwd.[(string (v))]) + d
                    if new_cost < old_cost then
                        q <- Array.append q [|{ node = nxt; cost = new_cost }|]
                        cst_fwd.[nxt] <- new_cost
                        parent.[nxt] <- v
                    if visited_backward.ContainsKey(nxt) then
                        let alt: int = ((cst_fwd.[(string (v))]) + d) + (cst_bwd.[(string (nxt))])
                        if alt < sd then
                            sd <- alt
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { queue = q; dist = sd }
        raise Return
        __ret
    with
        | Return -> __ret
let rec bidirectional_dij (source: string) (destination: string) (graph_forward: System.Collections.Generic.IDictionary<string, Edge array>) (graph_backward: System.Collections.Generic.IDictionary<string, Edge array>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable source = source
    let mutable destination = destination
    let mutable graph_forward = graph_forward
    let mutable graph_backward = graph_backward
    try
        let mutable shortest_path_distance: int = -1
        let mutable visited_forward: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let mutable visited_backward: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let mutable cst_fwd: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        cst_fwd.[source] <- 0
        let mutable cst_bwd: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        cst_bwd.[destination] <- 0
        let mutable parent_forward: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        parent_forward.[source] <- ""
        let mutable parent_backward: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        parent_backward.[destination] <- ""
        let mutable queue_forward: QItem array = [||]
        queue_forward <- Array.append queue_forward [|{ node = source; cost = 0 }|]
        let mutable queue_backward: QItem array = [||]
        queue_backward <- Array.append queue_backward [|{ node = destination; cost = 0 }|]
        let mutable shortest_distance: int = 2147483647
        if source = destination then
            __ret <- 0
            raise Return
        try
            while ((Seq.length (queue_forward)) > 0) && ((Seq.length (queue_backward)) > 0) do
                try
                    let idx_f: int = get_min_index (queue_forward)
                    let item_f: QItem = _idx queue_forward (idx_f)
                    queue_forward <- remove_at (queue_forward) (idx_f)
                    let v_fwd: string = item_f.node
                    visited_forward.[v_fwd] <- true
                    let idx_b: int = get_min_index (queue_backward)
                    let item_b: QItem = _idx queue_backward (idx_b)
                    queue_backward <- remove_at (queue_backward) (idx_b)
                    let v_bwd: string = item_b.node
                    visited_backward.[v_bwd] <- true
                    let res_f: PassResult = pass_and_relaxation (graph_forward) (v_fwd) (visited_forward) (visited_backward) (cst_fwd) (cst_bwd) (queue_forward) (parent_forward) (shortest_distance)
                    queue_forward <- res_f.queue
                    shortest_distance <- res_f.dist
                    let res_b: PassResult = pass_and_relaxation (graph_backward) (v_bwd) (visited_backward) (visited_forward) (cst_bwd) (cst_fwd) (queue_backward) (parent_backward) (shortest_distance)
                    queue_backward <- res_b.queue
                    shortest_distance <- res_b.dist
                    if ((cst_fwd.[(string (v_fwd))]) + (cst_bwd.[(string (v_bwd))])) >= shortest_distance then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if shortest_distance <> 2147483647 then
            shortest_path_distance <- shortest_distance
        __ret <- shortest_path_distance
        raise Return
        __ret
    with
        | Return -> __ret
let mutable graph_fwd: System.Collections.Generic.IDictionary<string, Edge array> = _dictCreate [("B", [|{ ``to`` = "C"; cost = 1 }|]); ("C", [|{ ``to`` = "D"; cost = 1 }|]); ("D", [|{ ``to`` = "F"; cost = 1 }|]); ("E", [|{ ``to`` = "B"; cost = 1 }; { ``to`` = "G"; cost = 2 }|]); ("F", Array.empty<Edge>); ("G", [|{ ``to`` = "F"; cost = 1 }|])]
let mutable graph_bwd: System.Collections.Generic.IDictionary<string, Edge array> = _dictCreate [("B", [|{ ``to`` = "E"; cost = 1 }|]); ("C", [|{ ``to`` = "B"; cost = 1 }|]); ("D", [|{ ``to`` = "C"; cost = 1 }|]); ("F", [|{ ``to`` = "D"; cost = 1 }; { ``to`` = "G"; cost = 1 }|]); ("E", Array.empty<Edge>); ("G", [|{ ``to`` = "E"; cost = 2 }|])]
printfn "%s" (_str (bidirectional_dij ("E") ("F") (graph_fwd) (graph_bwd)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
