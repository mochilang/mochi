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
type Edge = {
    mutable _u: int
    mutable _v: int
    mutable _w: int
}
type Graph = {
    mutable _edges: Edge array
    mutable _num_nodes: int
}
type DS = {
    mutable _parent: int array
    mutable _rank: int array
}
type FindResult = {
    mutable _ds: DS
    mutable _root: int
}
let rec new_graph () =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    try
        __ret <- { _edges = [||]; _num_nodes = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and add_edge (g: Graph) (_u: int) (_v: int) (_w: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    let mutable _u = _u
    let mutable _v = _v
    let mutable _w = _w
    try
        let mutable es: Edge array = g._edges
        es <- Array.append es [|{ _u = _u; _v = _v; _w = _w }|]
        let mutable n: int = g._num_nodes
        if _u > n then
            n <- _u
        if _v > n then
            n <- _v
        __ret <- { _edges = es; _num_nodes = n }
        raise Return
        __ret
    with
        | Return -> __ret
and make_ds (n: int) =
    let mutable __ret : DS = Unchecked.defaultof<DS>
    let mutable n = n
    try
        let mutable _parent: int array = [||]
        let mutable _rank: int array = [||]
        let mutable i: int = 0
        while i <= n do
            _parent <- Array.append _parent [|i|]
            _rank <- Array.append _rank [|0|]
            i <- i + 1
        __ret <- { _parent = _parent; _rank = _rank }
        raise Return
        __ret
    with
        | Return -> __ret
and find_set (_ds: DS) (x: int) =
    let mutable __ret : FindResult = Unchecked.defaultof<FindResult>
    let mutable _ds = _ds
    let mutable x = x
    try
        if (_idx (_ds._parent) (x)) = x then
            __ret <- { _ds = _ds; _root = x }
            raise Return
        let res: FindResult = find_set (_ds) (_idx (_ds._parent) (x))
        let mutable p: int array = (res._ds)._parent
        p.[x] <- res._root
        __ret <- { _ds = { _parent = p; _rank = (res._ds)._rank }; _root = res._root }
        raise Return
        __ret
    with
        | Return -> __ret
and union_set (_ds: DS) (x: int) (y: int) =
    let mutable __ret : DS = Unchecked.defaultof<DS>
    let mutable _ds = _ds
    let mutable x = x
    let mutable y = y
    try
        let fx: FindResult = find_set (_ds) (x)
        let ds1: DS = fx._ds
        let x_root: int = fx._root
        let fy: FindResult = find_set (ds1) (y)
        let mutable ds2: DS = fy._ds
        let y_root: int = fy._root
        if x_root = y_root then
            __ret <- ds2
            raise Return
        let mutable p: int array = ds2._parent
        let mutable r: int array = ds2._rank
        if (_idx r (x_root)) > (_idx r (y_root)) then
            p.[y_root] <- x_root
        else
            p.[x_root] <- y_root
            if (_idx r (x_root)) = (_idx r (y_root)) then
                r.[y_root] <- (_idx r (y_root)) + 1
        __ret <- { _parent = p; _rank = r }
        raise Return
        __ret
    with
        | Return -> __ret
and sort_edges (_edges: Edge array) =
    let mutable __ret : Edge array = Unchecked.defaultof<Edge array>
    let mutable _edges = _edges
    try
        let mutable arr: Edge array = _edges
        let mutable i: int = 1
        try
            while i < (Seq.length (arr)) do
                try
                    let key: Edge = _idx arr (i)
                    let mutable j: int = i - 1
                    try
                        while j >= 0 do
                            try
                                let temp: Edge = _idx arr (j)
                                if ((temp._w) > (key._w)) || (((temp._w) = (key._w)) && (((temp._u) > (key._u)) || (((temp._u) = (key._u)) && ((temp._v) > (key._v))))) then
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
                    arr.[j + 1] <- key
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
and kruskal (g: Graph) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable g = g
    try
        let mutable _edges: Edge array = sort_edges (g._edges)
        let mutable _ds: DS = make_ds (g._num_nodes)
        let mutable mst_edges: Edge array = [||]
        let mutable i: int = 0
        let mutable added: int = 0
        while (added < ((g._num_nodes) - 1)) && (i < (Seq.length (_edges))) do
            let e: Edge = _idx _edges (i)
            i <- i + 1
            let fu: FindResult = find_set (_ds) (e._u)
            _ds <- fu._ds
            let ru: int = fu._root
            let fv: FindResult = find_set (_ds) (e._v)
            _ds <- fv._ds
            let rv: int = fv._root
            if ru <> rv then
                mst_edges <- Array.append mst_edges [|e|]
                added <- added + 1
                _ds <- union_set (_ds) (ru) (rv)
        __ret <- { _edges = mst_edges; _num_nodes = g._num_nodes }
        raise Return
        __ret
    with
        | Return -> __ret
and print_mst (g: Graph) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        let mutable es: Edge array = sort_edges (g._edges)
        for e in es do
            printfn "%s" (((((_str (e._u)) + "-") + (_str (e._v))) + ":") + (_str (e._w)))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable g: Graph = new_graph()
        g <- add_edge (g) (1) (2) (1)
        g <- add_edge (g) (2) (3) (2)
        g <- add_edge (g) (3) (4) (1)
        g <- add_edge (g) (3) (5) (100)
        g <- add_edge (g) (4) (5) (5)
        let mst: Graph = kruskal (g)
        print_mst (mst)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
