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
type Pair = {
    mutable _a: string
    mutable _b: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let mutable _seed: int = 1
let rec rand_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- ((_seed % n + n) % n)
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains (list: string array) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (list)) do
            if (_idx list (i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_all (list: string array) (value: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable list = list
    let mutable value = value
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (list)) do
            if (_idx list (i)) <> value then
                res <- Array.append res [|(_idx list (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec partition_graph (graph: System.Collections.Generic.IDictionary<string, string array>) =
    let mutable __ret : Pair array = Unchecked.defaultof<Pair array>
    let mutable graph = graph
    try
        let mutable contracted: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for node in graph.Keys do
            contracted.[node] <- [|node|]
        let mutable graph_copy: System.Collections.Generic.IDictionary<string, string array> = _dictCreate []
        for node in graph.Keys do
            let mutable lst: string array = [||]
            let mutable neigh: string array = _dictGet graph ((string (node)))
            let mutable i: int = 0
            while i < (Seq.length (neigh)) do
                lst <- Array.append lst [|(_idx neigh (i))|]
                i <- i + 1
            graph_copy.[node] <- lst
        let mutable nodes: obj = box (graph_copy.Keys)
        while (Seq.length (nodes)) > 2 do
            let u: obj = _idx nodes (rand_int (Seq.length (nodes)))
            let u_neighbors: string array = _dictGet graph_copy ((string (u)))
            let v: string = _idx u_neighbors (rand_int (Seq.length (u_neighbors)))
            let uv: string = (unbox<string> u) + v
            let mutable uv_neighbors: string array = [||]
            let mutable i: int = 0
            while i < (Seq.length (_dictGet graph_copy ((string (u))))) do
                let n: string = _idx (_dictGet graph_copy ((string (u)))) (i)
                if ((n <> (unbox<string> u)) && (n <> v)) && ((contains (uv_neighbors) (n)) = false) then
                    uv_neighbors <- Array.append uv_neighbors [|n|]
                i <- i + 1
            i <- 0
            while i < (Seq.length (_dictGet graph_copy ((string (v))))) do
                let n: string = _idx (_dictGet graph_copy ((string (v)))) (i)
                if ((n <> (unbox<string> u)) && (n <> v)) && ((contains (uv_neighbors) (n)) = false) then
                    uv_neighbors <- Array.append uv_neighbors [|n|]
                i <- i + 1
            graph_copy.[uv] <- uv_neighbors
            let mutable k: int = 0
            while k < (Seq.length (uv_neighbors)) do
                let nb: string = _idx uv_neighbors (k)
                graph_copy.[nb] <- Array.append (_dictGet graph_copy ((string (nb)))) [|uv|]
                graph_copy.[nb] <- remove_all (_dictGet graph_copy ((string (nb)))) (unbox<string> u)
                graph_copy.[nb] <- remove_all (_dictGet graph_copy ((string (nb)))) (v)
                k <- k + 1
            let mutable group: string array = [||]
            i <- 0
            while i < (Seq.length (_dictGet contracted ((string (u))))) do
                group <- Array.append group [|(_idx (_dictGet contracted ((string (u)))) (i))|]
                i <- i + 1
            i <- 0
            while i < (Seq.length (_dictGet contracted ((string (v))))) do
                let ``val``: string = _idx (_dictGet contracted ((string (v)))) (i)
                if (contains (group) (``val``)) = false then
                    group <- Array.append group [|``val``|]
                i <- i + 1
            contracted.[uv] <- group
            nodes <- box (remove_all (unbox<string array> nodes) (unbox<string> u))
            nodes <- box (remove_all (unbox<string array> nodes) (v))
            nodes <- Array.append nodes [|uv|]
        let mutable groups: string array array = [||]
        let mutable j: int = 0
        while j < (int ((unbox<System.Array> nodes).Length)) do
            let n: obj = box (((nodes :?> System.Array).GetValue(j)))
            groups <- Array.append groups [|(_dictGet contracted ((string (n))))|]
            j <- j + 1
        let groupA: string array = _idx groups (0)
        let groupB: string array = _idx groups (1)
        let mutable cut: Pair array = [||]
        j <- 0
        while j < (Seq.length (groupA)) do
            let node: string = _idx groupA (j)
            let mutable neigh: string array = _dictGet graph ((string (node)))
            let mutable l: int = 0
            while l < (Seq.length (neigh)) do
                let nb: string = _idx neigh (l)
                if contains (groupB) (nb) then
                    cut <- Array.append cut [|{ _a = node; _b = nb }|]
                l <- l + 1
            j <- j + 1
        __ret <- cut
        raise Return
        __ret
    with
        | Return -> __ret
let rec cut_to_string (cut: Pair array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cut = cut
    try
        let mutable s: string = "{"
        let mutable i: int = 0
        while i < (Seq.length (cut)) do
            let p: Pair = _idx cut (i)
            s <- ((((s + "(") + (p._a)) + ", ") + (p._b)) + ")"
            if i < ((Seq.length (cut)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "}"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let TEST_GRAPH: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("1", [|"2"; "3"; "4"; "5"|]); ("2", [|"1"; "3"; "4"; "5"|]); ("3", [|"1"; "2"; "4"; "5"; "10"|]); ("4", [|"1"; "2"; "3"; "5"; "6"|]); ("5", [|"1"; "2"; "3"; "4"; "7"|]); ("6", [|"7"; "8"; "9"; "10"; "4"|]); ("7", [|"6"; "8"; "9"; "10"; "5"|]); ("8", [|"6"; "7"; "9"; "10"|]); ("9", [|"6"; "7"; "8"; "10"|]); ("10", [|"6"; "7"; "8"; "9"; "3"|])]
let result: Pair array = partition_graph (TEST_GRAPH)
printfn "%s" (cut_to_string (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
