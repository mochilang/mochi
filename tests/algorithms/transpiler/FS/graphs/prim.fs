// Generated 2025-08-08 16:03 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let INF: int = 1000000000
let rec connect (graph: System.Collections.Generic.IDictionary<int, int array array>) (a: int) (b: int) (w: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int array array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int array array>>
    let mutable graph = graph
    let mutable a = a
    let mutable b = b
    let mutable w = w
    try
        let mutable u: int = a - 1
        let v: int = b - 1
        let mutable g: System.Collections.Generic.IDictionary<int, int array array> = graph
        g.[u] <- Array.append (_dictGet g (u)) [|[|v; w|]|]
        g.[v] <- Array.append (_dictGet g (v)) [|[|u; w|]|]
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec in_list (arr: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec prim (graph: System.Collections.Generic.IDictionary<int, int array array>) (s: int) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    let mutable s = s
    let mutable n = n
    try
        let mutable dist: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable parent: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        dist.[s] <- 0
        parent.[s] <- -1
        let mutable known: int array = [||]
        let mutable keys: int array = [|s|]
        while (Seq.length (known)) < n do
            let mutable mini: int = INF
            let mutable u: int = -1
            let mutable i: int = 0
            while i < (Seq.length (keys)) do
                let k: int = _idx keys (i)
                let d: int = _dictGet dist (k)
                if (not (in_list (known) (k))) && (d < mini) then
                    mini <- d
                    u <- k
                i <- i + 1
            known <- Array.append known [|u|]
            for e in (_dictGet graph (u)).Keys do
                let v: int = _idx e (0)
                let w: int = _idx e (1)
                if not (in_list (keys) (v)) then
                    keys <- Array.append keys [|v|]
                let cur: int = if dist.ContainsKey(v) then (_dictGet dist (v)) else INF
                if (not (in_list (known) (v))) && (w < cur) then
                    dist.[v] <- w
                    parent.[v] <- u
        let mutable edges: int array array = [||]
        let mutable j: int = 0
        while j < (Seq.length (keys)) do
            let v: int = _idx keys (j)
            if v <> s then
                edges <- Array.append edges [|[|v + 1; (_dictGet parent (v)) + 1|]|]
            j <- j + 1
        __ret <- edges
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_heap (h: int array) (dist: System.Collections.Generic.IDictionary<int, int>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable h = h
    let mutable dist = dist
    try
        let mutable a: int array = h
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable j: int = 0
            while j < (((Seq.length (a)) - i) - 1) do
                let dj: int = if dist.ContainsKey((_idx a (j))) then (_dictGet dist (_idx a (j))) else INF
                let dj1: int = if dist.ContainsKey((_idx a (j + 1))) then (_dictGet dist (_idx a (j + 1))) else INF
                if dj > dj1 then
                    let t: int = _idx a (j)
                    a.[j] <- _idx a (j + 1)
                    a.[j + 1] <- t
                j <- j + 1
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
let rec prim_heap (graph: System.Collections.Generic.IDictionary<int, int array array>) (s: int) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    let mutable s = s
    let mutable n = n
    try
        let mutable dist: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable parent: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        dist.[s] <- 0
        parent.[s] <- -1
        let mutable h: int array = [||]
        let mutable i: int = 0
        while i < n do
            h <- Array.append h [|i|]
            i <- i + 1
        h <- sort_heap (h) (dist)
        let mutable known: int array = [||]
        while (Seq.length (h)) > 0 do
            let mutable u: int = _idx h (0)
            h <- Array.sub h 1 ((Seq.length (h)) - 1)
            known <- Array.append known [|u|]
            for e in (_dictGet graph (u)).Keys do
                let v: int = _idx e (0)
                let w: int = _idx e (1)
                let cur: int = if dist.ContainsKey(v) then (_dictGet dist (v)) else INF
                if (not (in_list (known) (v))) && (w < cur) then
                    dist.[v] <- w
                    parent.[v] <- u
            h <- sort_heap (h) (dist)
        let mutable edges: int array array = [||]
        let mutable j: int = 0
        while j < n do
            if j <> s then
                edges <- Array.append edges [|[|j + 1; (_dictGet parent (j)) + 1|]|]
            j <- j + 1
        __ret <- edges
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_edges (edges: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable edges = edges
    try
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let e: int array = _idx edges (i)
            printfn "%s" (((("(" + (_str (_idx e (0)))) + ", ") + (_str (_idx e (1)))) + ")")
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec test_vector () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let x: int = 5
        let mutable G: System.Collections.Generic.IDictionary<int, int array array> = _dictCreate []
        let mutable i: int = 0
        while i < x do
            G.[i] <- [||]
            i <- i + 1
        G <- connect (G) (1) (2) (15)
        G <- connect (G) (1) (3) (12)
        G <- connect (G) (2) (4) (13)
        G <- connect (G) (2) (5) (5)
        G <- connect (G) (3) (2) (6)
        G <- connect (G) (3) (4) (6)
        let mst: int array array = prim (G) (0) (x)
        print_edges (mst)
        let mst_heap: int array array = prim_heap (G) (0) (x)
        print_edges (mst_heap)
        __ret
    with
        | Return -> __ret
test_vector()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
