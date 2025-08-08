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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type QueueNode = {
    mutable _node: string
    mutable _weight: int
}
type MSTResult = {
    mutable _dist: System.Collections.Generic.IDictionary<string, int>
    mutable _parent: System.Collections.Generic.IDictionary<string, string>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec prims_algo (graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>>) =
    let mutable __ret : MSTResult = Unchecked.defaultof<MSTResult>
    let mutable graph = graph
    try
        let INF: int = 2147483647
        let mutable _dist: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable _parent: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
        let mutable queue: QueueNode array = [||]
        for _node in graph.Keys do
            _dist.[_node] <- INF
            _parent.[_node] <- ""
            queue <- Array.append queue [|{ _node = _node; _weight = INF }|]
        if (Seq.length (queue)) = 0 then
            __ret <- { _dist = _dist; _parent = _parent }
            raise Return
        let mutable min_idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (queue)) do
            if ((_idx queue (i))._weight) < ((_idx queue (min_idx))._weight) then
                min_idx <- i
            i <- i + 1
        let start_node: QueueNode = _idx queue (min_idx)
        let start: string = start_node._node
        let mutable new_q: QueueNode array = [||]
        let mutable j: int = 0
        while j < (Seq.length (queue)) do
            if j <> min_idx then
                new_q <- Array.append new_q [|(_idx queue (j))|]
            j <- j + 1
        queue <- new_q
        _dist.[start] <- 0
        try
            for neighbour in (_dictGet graph (start)).Keys do
                try
                    let w: int = _dictGet graph (start).[neighbour]
                    if (_dictGet _dist ((string (neighbour)))) > ((_dictGet _dist ((string (start)))) + w) then
                        _dist.[neighbour] <- (_dictGet _dist ((string (start)))) + w
                        _parent.[neighbour] <- start
                        let mutable k: int = 0
                        try
                            while k < (Seq.length (queue)) do
                                try
                                    if ((_idx queue (k))._node) = (unbox<string> neighbour) then
                                        queue.[k]._weight <- _dictGet _dist ((string (neighbour)))
                                        raise Break
                                    k <- k + 1
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
        try
            while (Seq.length (queue)) > 0 do
                try
                    let mutable best_idx: int = 0
                    let mutable p: int = 1
                    while p < (Seq.length (queue)) do
                        if ((_idx queue (p))._weight) < ((_idx queue (best_idx))._weight) then
                            best_idx <- p
                        p <- p + 1
                    let node_entry: QueueNode = _idx queue (best_idx)
                    let _node: string = node_entry._node
                    let mutable tmp: QueueNode array = [||]
                    let mutable q: int = 0
                    while q < (Seq.length (queue)) do
                        if q <> best_idx then
                            tmp <- Array.append tmp [|(_idx queue (q))|]
                        q <- q + 1
                    queue <- tmp
                    try
                        for neighbour in (_dictGet graph (_node)).Keys do
                            try
                                let w: int = _dictGet graph (_node).[neighbour]
                                if (_dictGet _dist ((string (neighbour)))) > ((_dictGet _dist ((string (_node)))) + w) then
                                    _dist.[neighbour] <- (_dictGet _dist ((string (_node)))) + w
                                    _parent.[neighbour] <- _node
                                    let mutable r: int = 0
                                    try
                                        while r < (Seq.length (queue)) do
                                            try
                                                if ((_idx queue (r))._node) = (unbox<string> neighbour) then
                                                    queue.[r]._weight <- _dictGet _dist ((string (neighbour)))
                                                    raise Break
                                                r <- r + 1
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
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _dist = _dist; _parent = _parent }
        raise Return
        __ret
    with
        | Return -> __ret
let rec iabs (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let mutable graph: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>> = _dictCreate []
graph.["a"] <- _dictCreate [("b", 3); ("c", 15)]
graph.["b"] <- _dictCreate [("a", 3); ("c", 10); ("d", 100)]
graph.["c"] <- _dictCreate [("a", 15); ("b", 10); ("d", 5)]
graph.["d"] <- _dictCreate [("b", 100); ("c", 5)]
let res: MSTResult = prims_algo (graph)
let _dist: System.Collections.Generic.IDictionary<string, int> = res._dist
printfn "%s" (_str (iabs ((_dictGet _dist ((string ("a")))) - (_dictGet _dist ((string ("b")))))))
printfn "%s" (_str (iabs ((_dictGet _dist ((string ("d")))) - (_dictGet _dist ((string ("b")))))))
printfn "%s" (_str (iabs ((_dictGet _dist ((string ("a")))) - (_dictGet _dist ((string ("c")))))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
