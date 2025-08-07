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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec dfs (g: System.Collections.Generic.IDictionary<int, int array>) (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable s = s
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable stack: int array = [||]
        visited.[s] <- true
        stack <- Array.append stack [|s|]
        printfn "%d" (s)
        try
            while (Seq.length (stack)) > 0 do
                try
                    let mutable u: int = _idx stack ((Seq.length (stack)) - 1)
                    let mutable found: bool = false
                    try
                        for v in g.[u] do
                            try
                                if not (visited.ContainsKey(v)) then
                                    visited.[v] <- true
                                    stack <- Array.append stack [|v|]
                                    printfn "%d" (v)
                                    found <- true
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
let rec bfs (g: System.Collections.Generic.IDictionary<int, int array>) (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable s = s
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable q: int array = [||]
        visited.[s] <- true
        q <- Array.append q [|s|]
        printfn "%d" (s)
        while (Seq.length (q)) > 0 do
            let mutable u: int = _idx q (0)
            q <- Array.sub q 1 ((Seq.length (q)) - 1)
            for v in g.[u] do
                if not (visited.ContainsKey(v)) then
                    visited.[v] <- true
                    q <- Array.append q [|v|]
                    printfn "%d" (v)
        __ret
    with
        | Return -> __ret
let rec sort_ints (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable arr: int array = a
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = 0
            while j < (((Seq.length (arr)) - i) - 1) do
                if (_idx arr (j)) > (_idx arr (j + 1)) then
                    let tmp: int = _idx arr (j)
                    arr.[j] <- _idx arr (j + 1)
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec dijkstra (g: System.Collections.Generic.IDictionary<int, int array array>) (s: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable s = s
    try
        let mutable dist: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        dist.[s] <- 0
        let mutable path: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        path.[s] <- 0
        let mutable known: int array = [||]
        let mutable keys: int array = [|s|]
        while (Seq.length (known)) < (Seq.length (keys)) do
            let mutable mini: int = 100000
            let mutable u: int = -1
            let mutable i: int = 0
            while i < (Seq.length (keys)) do
                let mutable k: int = _idx keys (i)
                let d: int = dist.[k]
                if (not (Seq.contains k known)) && (d < mini) then
                    mini <- d
                    u <- k
                i <- i + 1
            known <- Array.append known [|u|]
            for e in g.[u] do
                let v: int = _idx e (0)
                let w: int = _idx e (1)
                if not (Seq.contains v keys) then
                    keys <- Array.append keys [|v|]
                let alt: int = (dist.[u]) + w
                let cur: int = if dist.ContainsKey(v) then (dist.[v]) else 100000
                if (not (Seq.contains v known)) && (alt < cur) then
                    dist.[v] <- alt
                    path.[v] <- u
        let ordered: int array = sort_ints (keys)
        let mutable idx: int = 0
        while idx < (Seq.length (ordered)) do
            let mutable k: int = _idx ordered (idx)
            if k <> s then
                printfn "%d" (dist.[k])
            idx <- idx + 1
        __ret
    with
        | Return -> __ret
let rec topo (g: System.Collections.Generic.IDictionary<int, int array>) (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable n = n
    try
        let mutable ind: int array = [||]
        let mutable i: int = 0
        while i <= n do
            ind <- Array.append ind [|0|]
            i <- i + 1
        let mutable node: int = 1
        while node <= n do
            for v in g.[node] do
                ind.[v] <- (_idx ind (v)) + 1
            node <- node + 1
        let mutable q: int array = [||]
        let mutable j: int = 1
        while j <= n do
            if (_idx ind (j)) = 0 then
                q <- Array.append q [|j|]
            j <- j + 1
        while (Seq.length (q)) > 0 do
            let v: int = _idx q (0)
            q <- Array.sub q 1 ((Seq.length (q)) - 1)
            printfn "%d" (v)
            for w in g.[v] do
                ind.[w] <- (_idx ind (w)) - 1
                if (_idx ind (w)) = 0 then
                    q <- Array.append q [|w|]
        __ret
    with
        | Return -> __ret
let rec floyd (a: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    try
        let n: int = Seq.length (a)
        let mutable dist: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(_idx (_idx a (i)) (j))|]
                j <- j + 1
            dist <- Array.append dist [|row|]
            i <- i + 1
        let mutable k: int = 0
        while k < n do
            let mutable ii: int = 0
            while ii < n do
                let mutable jj: int = 0
                while jj < n do
                    if (_idx (_idx dist (ii)) (jj)) > ((_idx (_idx dist (ii)) (k)) + (_idx (_idx dist (k)) (jj))) then
                        dist.[ii].[jj] <- (_idx (_idx dist (ii)) (k)) + (_idx (_idx dist (k)) (jj))
                    jj <- jj + 1
                ii <- ii + 1
            k <- k + 1
        printfn "%s" (_repr (dist))
        __ret
    with
        | Return -> __ret
let rec prim (g: System.Collections.Generic.IDictionary<int, int array array>) (s: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable s = s
    let mutable n = n
    try
        let mutable dist: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        dist.[s] <- 0
        let mutable known: int array = [||]
        let mutable keys: int array = [|s|]
        let mutable total: int = 0
        while (Seq.length (known)) < n do
            let mutable mini: int = 100000
            let mutable u: int = -1
            let mutable i: int = 0
            while i < (Seq.length (keys)) do
                let mutable k: int = _idx keys (i)
                let d: int = dist.[k]
                if (not (Seq.contains k known)) && (d < mini) then
                    mini <- d
                    u <- k
                i <- i + 1
            known <- Array.append known [|u|]
            total <- total + mini
            for e in g.[u] do
                let v: int = _idx e (0)
                let w: int = _idx e (1)
                if not (Seq.contains v keys) then
                    keys <- Array.append keys [|v|]
                let cur: int = if dist.ContainsKey(v) then (dist.[v]) else 100000
                if (not (Seq.contains v known)) && (w < cur) then
                    dist.[v] <- w
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_edges (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable edges = edges
    try
        let mutable es: int array array = edges
        let mutable i: int = 0
        while i < (Seq.length (es)) do
            let mutable j: int = 0
            while j < (((Seq.length (es)) - i) - 1) do
                if (_idx (_idx es (j)) (2)) > (_idx (_idx es (j + 1)) (2)) then
                    let tmp: int array = _idx es (j)
                    es.[j] <- _idx es (j + 1)
                    es.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- es
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_parent (parent: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable parent = parent
    let mutable x = x
    try
        let mutable r: int = x
        while (_idx parent (r)) <> r do
            r <- _idx parent (r)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec union_parent (parent: int array) (a: int) (b: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable parent = parent
    let mutable a = a
    let mutable b = b
    try
        parent.[a] <- b
        __ret
    with
        | Return -> __ret
let rec kruskal (edges: int array array) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable edges = edges
    let mutable n = n
    try
        let mutable es: int array array = sort_edges (edges)
        let mutable parent: int array = [||]
        let mutable i: int = 0
        while i <= n do
            parent <- Array.append parent [|i|]
            i <- i + 1
        let mutable total: int = 0
        let mutable count: int = 0
        let mutable idx: int = 0
        while (count < (n - 1)) && (idx < (Seq.length (es))) do
            let e: int array = _idx es (idx)
            idx <- idx + 1
            let mutable u: int = _idx e (0)
            let v: int = _idx e (1)
            let w: int = _idx e (2)
            let ru: int = find_parent (parent) (u)
            let rv: int = find_parent (parent) (v)
            if ru <> rv then
                union_parent (parent) (ru) (rv)
                total <- total + w
                count <- count + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_isolated_nodes (g: System.Collections.Generic.IDictionary<int, int array>) (nodes: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable nodes = nodes
    try
        let mutable isolated: int array = [||]
        for node in nodes do
            if (Seq.length (g.[node])) = 0 then
                isolated <- Array.append isolated [|node|]
        __ret <- isolated
        raise Return
        __ret
    with
        | Return -> __ret
let g_dfs: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|4; 5|]); (3, Array.empty<int>); (4, Array.empty<int>); (5, Array.empty<int>)]
let g_bfs: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|4; 5|]); (3, [|6; 7|]); (4, Array.empty<int>); (5, [|8|]); (6, Array.empty<int>); (7, Array.empty<int>); (8, Array.empty<int>)]
let g_weighted: System.Collections.Generic.IDictionary<int, int array array> = _dictCreate [(1, [|[|2; 7|]; [|3; 9|]; [|6; 14|]|]); (2, [|[|1; 7|]; [|3; 10|]; [|4; 15|]|]); (3, [|[|1; 9|]; [|2; 10|]; [|4; 11|]; [|6; 2|]|]); (4, [|[|2; 15|]; [|3; 11|]; [|5; 6|]|]); (5, [|[|4; 6|]; [|6; 9|]|]); (6, [|[|1; 14|]; [|3; 2|]; [|5; 9|]|])]
let g_topo: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|4|]); (3, [|4|]); (4, Array.empty<int>)]
let matrix: int array array = [|[|0; 5; 9; 100000|]; [|100000; 0; 2; 8|]; [|100000; 100000; 0; 7|]; [|4; 100000; 100000; 0|]|]
let g_prim: System.Collections.Generic.IDictionary<int, int array array> = _dictCreate [(1, [|[|2; 1|]; [|3; 3|]|]); (2, [|[|1; 1|]; [|3; 1|]; [|4; 6|]|]); (3, [|[|1; 3|]; [|2; 1|]; [|4; 2|]|]); (4, [|[|2; 6|]; [|3; 2|]|])]
let edges_kruskal: int array array = [|[|1; 2; 1|]; [|2; 3; 2|]; [|1; 3; 2|]; [|3; 4; 1|]|]
let g_iso: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|1; 3|]); (3, [|1; 2|]); (4, Array.empty<int>)]
dfs (g_dfs) (1)
bfs (g_bfs) (1)
dijkstra (g_weighted) (1)
topo (g_topo) (4)
floyd (matrix)
printfn "%d" (prim (g_prim) (1) (4))
printfn "%d" (kruskal (edges_kruskal) (4))
let iso: int array = find_isolated_nodes (g_iso) (unbox<int array> [|1; 2; 3; 4|])
printfn "%s" (_repr (iso))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
