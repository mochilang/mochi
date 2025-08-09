// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec bfs (graph: int array array) (s: int) (t: int) (parent: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable s = s
    let mutable t = t
    let mutable parent = parent
    try
        let mutable visited: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < (Seq.length (graph)) do
            visited <- Array.append visited [|false|]
            i <- i + 1
        let mutable queue: int array = unbox<int array> [|s|]
        let mutable head: int = 0
        visited.[int s] <- true
        while head < (Seq.length (queue)) do
            let u: int = _idx queue (int head)
            head <- head + 1
            let mutable ind: int = 0
            while ind < (Seq.length (_idx graph (int u))) do
                if ((_idx visited (int ind)) = false) && ((_idx (_idx graph (int u)) (int ind)) > 0) then
                    queue <- Array.append queue [|ind|]
                    visited.[int ind] <- true
                    parent.[int ind] <- u
                ind <- ind + 1
        __ret <- _idx visited (int t)
        raise Return
        __ret
    with
        | Return -> __ret
let rec mincut (graph: int array array) (source: int) (sink: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    let mutable source = source
    let mutable sink = sink
    try
        let mutable g: int array array = graph
        let mutable parent: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (g)) do
            parent <- Array.append parent [|(-1)|]
            i <- i + 1
        let mutable temp: int array array = Array.empty<int array>
        i <- 0
        while i < (Seq.length (g)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx g (int i))) do
                row <- Array.append row [|(_idx (_idx g (int i)) (int j))|]
                j <- j + 1
            temp <- Array.append temp [|row|]
            i <- i + 1
        while bfs (g) (source) (sink) (parent) do
            let mutable path_flow: int = 1000000000
            let mutable s: int = sink
            while s <> source do
                let p: int = _idx parent (int s)
                let cap: int = _idx (_idx g (int p)) (int s)
                if cap < path_flow then
                    path_flow <- cap
                s <- p
            let mutable v: int = sink
            while v <> source do
                let u: int = _idx parent (int v)
                g.[int u].[int v] <- (_idx (_idx g (int u)) (int v)) - path_flow
                g.[int v].[int u] <- (_idx (_idx g (int v)) (int u)) + path_flow
                v <- u
        let mutable res: int array array = Array.empty<int array>
        i <- 0
        while i < (Seq.length (g)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx g (int 0))) do
                if ((_idx (_idx g (int i)) (int j)) = 0) && ((_idx (_idx temp (int i)) (int j)) > 0) then
                    res <- Array.append res [|[|i; j|]|]
                j <- j + 1
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let test_graph: int array array = [|[|0; 16; 13; 0; 0; 0|]; [|0; 0; 10; 12; 0; 0|]; [|0; 4; 0; 0; 14; 0|]; [|0; 0; 9; 0; 0; 20|]; [|0; 0; 0; 7; 0; 4|]; [|0; 0; 0; 0; 0; 0|]|]
let result: int array array = mincut (test_graph) (0) (5)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
