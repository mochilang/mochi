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
    mutable ``to``: int
    mutable _weight: int
}
type Pair = {
    mutable _u: int
    mutable _v: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: int = 1000000000
let rec pairs_to_string (edges: Pair array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable edges = edges
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let e: Pair = _idx edges (i)
            s <- ((((s + "(") + (_str (e._u))) + ", ") + (_str (e._v))) + ")"
            if i < ((Seq.length (edges)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec prim_mst (graph: Edge array array) =
    let mutable __ret : Pair array = Unchecked.defaultof<Pair array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable visited: bool array = [||]
        let mutable dist: int array = [||]
        let mutable parent: int array = [||]
        let mutable i: int = 0
        while i < n do
            visited <- Array.append visited [|false|]
            dist <- Array.append dist [|INF|]
            parent <- Array.append parent [|(-1)|]
            i <- i + 1
        dist.[0] <- 0
        let mutable result: Pair array = [||]
        let mutable count: int = 0
        try
            while count < n do
                try
                    let mutable min_val: int = INF
                    let mutable _u: int = 0
                    let mutable _v: int = 0
                    while _v < n do
                        if ((_idx visited (_v)) = false) && ((_idx dist (_v)) < min_val) then
                            min_val <- _idx dist (_v)
                            _u <- _v
                        _v <- _v + 1
                    if min_val = INF then
                        raise Break
                    visited.[_u] <- true
                    if _u <> 0 then
                        result <- Array.append result [|{ _u = _idx parent (_u); _v = _u }|]
                    for e in _idx graph (_u) do
                        if ((_idx visited (e.``to``)) = false) && ((e._weight) < (_idx dist (e.``to``))) then
                            dist.[e.``to``] <- e._weight
                            parent.[e.``to``] <- _u
                    count <- count + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let adjacency_list: Edge array array = [|[|{ ``to`` = 1; _weight = 1 }; { ``to`` = 3; _weight = 3 }|]; [|{ ``to`` = 0; _weight = 1 }; { ``to`` = 2; _weight = 6 }; { ``to`` = 3; _weight = 5 }; { ``to`` = 4; _weight = 1 }|]; [|{ ``to`` = 1; _weight = 6 }; { ``to`` = 4; _weight = 5 }; { ``to`` = 5; _weight = 2 }|]; [|{ ``to`` = 0; _weight = 3 }; { ``to`` = 1; _weight = 5 }; { ``to`` = 4; _weight = 1 }|]; [|{ ``to`` = 1; _weight = 1 }; { ``to`` = 2; _weight = 5 }; { ``to`` = 3; _weight = 1 }; { ``to`` = 5; _weight = 4 }|]; [|{ ``to`` = 2; _weight = 2 }; { ``to`` = 4; _weight = 4 }|]|]
let mst_edges: Pair array = prim_mst (adjacency_list)
printfn "%s" (pairs_to_string (mst_edges))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
