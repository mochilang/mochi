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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type DfsResult = {
    mutable _id: int
    mutable _bridges: int array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec dfs (graph: System.Collections.Generic.IDictionary<int, int array>) (at: int) (parent: int) (visited: bool array) (ids: int array) (low: int array) (_id: int) (_bridges: int array array) =
    let mutable __ret : DfsResult = Unchecked.defaultof<DfsResult>
    let mutable graph = graph
    let mutable at = at
    let mutable parent = parent
    let mutable visited = visited
    let mutable ids = ids
    let mutable low = low
    let mutable _id = _id
    let mutable _bridges = _bridges
    try
        visited.[at] <- true
        ids.[at] <- _id
        low.[at] <- _id
        let mutable current_id: int = _id + 1
        let mutable res_bridges: int array array = _bridges
        try
            for ``to`` in (_dictGet graph (at)).Keys do
                try
                    if ``to`` = parent then
                        raise Continue
                    else
                        if not (_idx visited (``to``)) then
                            let result: DfsResult = dfs (graph) (``to``) (at) (visited) (ids) (low) (current_id) (res_bridges)
                            current_id <- result._id
                            res_bridges <- result._bridges
                            if (_idx low (at)) > (_idx low (``to``)) then
                                low.[at] <- _idx low (``to``)
                            if (_idx ids (at)) < (_idx low (``to``)) then
                                let edge = if at < ``to`` then [|at; ``to``|] else [|``to``; at|]
                                res_bridges <- Array.append res_bridges [|edge|]
                        else
                            if (_idx low (at)) > (_idx ids (``to``)) then
                                low.[at] <- _idx ids (``to``)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _id = current_id; _bridges = res_bridges }
        raise Return
        __ret
    with
        | Return -> __ret
let rec compute_bridges (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable visited: bool array = [||]
        let mutable ids: int array = [||]
        let mutable low: int array = [||]
        let mutable i: int = 0
        while i < n do
            visited <- Array.append visited [|false|]
            ids <- Array.append ids [|0|]
            low <- Array.append low [|0|]
            i <- i + 1
        let mutable _bridges: int array array = [||]
        let mutable _id: int = 0
        i <- 0
        while i < n do
            if not (_idx visited (i)) then
                let result: DfsResult = dfs (graph) (i) (-1) (visited) (ids) (low) (_id) (_bridges)
                _id <- result._id
                _bridges <- result._bridges
            i <- i + 1
        __ret <- _bridges
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_demo_graph (index: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int array>>
    let mutable index = index
    try
        if index = 0 then
            __ret <- unbox<System.Collections.Generic.IDictionary<int, int array>> (_dictCreate [(0, [|1; 2|]); (1, [|0; 2|]); (2, [|0; 1; 3; 5|]); (3, [|2; 4|]); (4, [|3|]); (5, [|2; 6; 8|]); (6, [|5; 7|]); (7, [|6; 8|]); (8, [|5; 7|])])
            raise Return
        if index = 1 then
            __ret <- unbox<System.Collections.Generic.IDictionary<int, int array>> (_dictCreate [(0, box ([|6|])); (1, box ([|9|])); (2, box ([|4; 5|])); (3, box ([|4|])); (4, box ([|2; 3|])); (5, box ([|2|])); (6, box ([|0; 7|])); (7, box ([|6|])); (8, box ([||])); (9, box ([|1|]))])
            raise Return
        if index = 2 then
            __ret <- unbox<System.Collections.Generic.IDictionary<int, int array>> (_dictCreate [(0, box ([|4|])); (1, box ([|6|])); (2, box ([||])); (3, box ([|5; 6; 7|])); (4, box ([|0; 6|])); (5, box ([|3; 8; 9|])); (6, box ([|1; 3; 4; 7|])); (7, box ([|3; 6; 8; 9|])); (8, box ([|5; 7|])); (9, box ([|5; 7|]))])
            raise Return
        __ret <- unbox<System.Collections.Generic.IDictionary<int, int array>> (_dictCreate [(0, [|1; 3|]); (1, [|0; 2; 4|]); (2, [|1; 3; 4|]); (3, [|0; 2; 4|]); (4, [|1; 2; 3|])])
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (compute_bridges (get_demo_graph (0))))
printfn "%s" (_repr (compute_bridges (get_demo_graph (1))))
printfn "%s" (_repr (compute_bridges (get_demo_graph (2))))
printfn "%s" (_repr (compute_bridges (get_demo_graph (3))))
printfn "%s" (_repr (compute_bridges (_dictCreate [])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
