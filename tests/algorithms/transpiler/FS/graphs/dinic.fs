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
type Dinic = {
    mutable _n: int
    mutable _lvl: int array
    mutable _ptr: int array
    mutable _q: int array
    mutable _adj: int array array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: int = 1000000000
let rec pow2 (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable k = k
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < k do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec min2 (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_dinic (_n: int) =
    let mutable __ret : Dinic = Unchecked.defaultof<Dinic>
    let mutable _n = _n
    try
        let mutable _lvl: int array = [||]
        let mutable _ptr: int array = [||]
        let mutable _q: int array = [||]
        let mutable _adj: int array array array = [||]
        let mutable i: int = 0
        while i < _n do
            _lvl <- Array.append _lvl [|0|]
            _ptr <- Array.append _ptr [|0|]
            _q <- Array.append _q [|0|]
            let mutable edges: int array array = [||]
            _adj <- Array.append _adj [|edges|]
            i <- i + 1
        __ret <- { _n = _n; _lvl = _lvl; _ptr = _ptr; _q = _q; _adj = _adj }
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_edge (g: Dinic) (a: int) (b: int) (c: int) (rcap: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable rcap = rcap
    try
        let mutable _adj: int array array array = g._adj
        let mutable list_a: int array array = _idx _adj (a)
        let mutable list_b: int array array = _idx _adj (b)
        let e1: int array = [|b; Seq.length (list_b); c; 0|]
        let e2: int array = [|a; Seq.length (list_a); rcap; 0|]
        list_a <- Array.append list_a [|e1|]
        list_b <- Array.append list_b [|e2|]
        _adj.[a] <- list_a
        _adj.[b] <- list_b
        g._adj <- _adj
        __ret
    with
        | Return -> __ret
let rec dfs (g: Dinic) (v: int) (sink: int) (flow: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable v = v
    let mutable sink = sink
    let mutable flow = flow
    try
        if (v = sink) || (flow = 0) then
            __ret <- flow
            raise Return
        let mutable _ptr: int array = g._ptr
        let mutable i: int = _idx _ptr (v)
        let mutable adj_all: int array array array = g._adj
        let mutable adj_v: int array array = _idx adj_all (v)
        while i < (Seq.length (adj_v)) do
            let mutable e: int array = _idx adj_v (i)
            let ``to``: int = _idx e (0)
            if (_idx (g._lvl) (``to``)) = ((_idx (g._lvl) (v)) + 1) then
                let avail: int = (_idx e (2)) - (_idx e (3))
                let pushed: int = dfs (g) (``to``) (sink) (min2 (flow) (avail))
                if pushed > 0 then
                    e.[3] <- (_idx e (3)) + pushed
                    adj_v.[i] <- e
                    let mutable adj_to: int array array = _idx adj_all (``to``)
                    let mutable back: int array = _idx adj_to (_idx e (1))
                    back.[3] <- (_idx back (3)) - pushed
                    adj_to.[_idx e (1)] <- back
                    adj_all.[``to``] <- adj_to
                    adj_all.[v] <- adj_v
                    g._adj <- adj_all
                    __ret <- pushed
                    raise Return
            i <- i + 1
            _ptr.[v] <- i
        g._ptr <- _ptr
        adj_all.[v] <- adj_v
        g._adj <- adj_all
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_flow (g: Dinic) (source: int) (sink: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable source = source
    let mutable sink = sink
    try
        let mutable flow: int = 0
        let mutable l: int = 0
        try
            while l < 31 do
                try
                    let threshold: int = pow2 (30 - l)
                    try
                        while true do
                            try
                                let mutable _lvl: int array = [||]
                                let mutable _ptr: int array = [||]
                                let mutable i: int = 0
                                while i < (g._n) do
                                    _lvl <- Array.append _lvl [|0|]
                                    _ptr <- Array.append _ptr [|0|]
                                    i <- i + 1
                                g._lvl <- _lvl
                                g._ptr <- _ptr
                                let mutable qi: int = 0
                                let mutable qe: int = 1
                                _lvl.[source] <- 1
                                g._lvl <- _lvl
                                let mutable _q: int array = g._q
                                _q.[0] <- source
                                while (qi < qe) && ((_idx (g._lvl) (sink)) = 0) do
                                    let mutable v: int = _idx _q (qi)
                                    qi <- qi + 1
                                    let mutable edges: int array array = _idx (g._adj) (v)
                                    let mutable j: int = 0
                                    while j < (Seq.length (edges)) do
                                        let e: int array = _idx edges (j)
                                        let ``to``: int = _idx e (0)
                                        let residual: int = (_idx e (2)) - (_idx e (3))
                                        let mutable lvl_inner: int array = g._lvl
                                        if ((_idx lvl_inner (``to``)) = 0) && (residual >= threshold) then
                                            _q.[qe] <- ``to``
                                            qe <- qe + 1
                                            lvl_inner.[``to``] <- (_idx lvl_inner (v)) + 1
                                            g._lvl <- lvl_inner
                                        j <- j + 1
                                let mutable p: int = dfs (g) (source) (sink) (INF)
                                while p > 0 do
                                    flow <- flow + p
                                    p <- dfs (g) (source) (sink) (INF)
                                if (_idx (g._lvl) (sink)) = 0 then
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    l <- l + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- flow
        raise Return
        __ret
    with
        | Return -> __ret
let mutable graph: Dinic = new_dinic (10)
let source: int = 0
let sink: int = 9
let mutable v: int = 1
while v < 5 do
    add_edge (graph) (source) (v) (1) (0)
    v <- v + 1
v <- 5
while v < 9 do
    add_edge (graph) (v) (sink) (1) (0)
    v <- v + 1
v <- 1
while v < 5 do
    add_edge (graph) (v) (v + 4) (1) (0)
    v <- v + 1
printfn "%s" (_str (max_flow (graph) (source) (sink)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
