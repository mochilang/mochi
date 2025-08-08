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
let rec topology_sort (graph: int array array) (vert: int) (visited: bool array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable vert = vert
    let mutable visited = visited
    try
        visited.[vert] <- true
        let mutable order: int array = [||]
        for neighbour in _idx graph (vert) do
            if not (_idx visited (neighbour)) then
                order <- unbox<int array> (Array.append (order) (topology_sort (graph) (neighbour) (visited)))
        order <- Array.append order [|vert|]
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and find_component (graph: int array array) (vert: int) (visited: bool array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable vert = vert
    let mutable visited = visited
    try
        visited.[vert] <- true
        let mutable comp: int array = [|vert|]
        for neighbour in _idx graph (vert) do
            if not (_idx visited (neighbour)) then
                comp <- unbox<int array> (Array.append (comp) (find_component (graph) (neighbour) (visited)))
        __ret <- comp
        raise Return
        __ret
    with
        | Return -> __ret
and strongly_connected_components (graph: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable visited: bool array = [||]
        for _ in 0 .. (n - 1) do
            visited <- Array.append visited [|false|]
        let mutable reversed: int array array = [||]
        for _ in 0 .. (n - 1) do
            reversed <- Array.append reversed [|[||]|]
        for i in 0 .. (n - 1) do
            for neighbour in _idx graph (i) do
                reversed.[neighbour] <- Array.append (_idx reversed (neighbour)) [|i|]
        let mutable order: int array = [||]
        for i in 0 .. (n - 1) do
            if not (_idx visited (i)) then
                order <- unbox<int array> (Array.append (order) (topology_sort (graph) (i) (visited)))
        visited <- Array.empty<bool>
        for _ in 0 .. (n - 1) do
            visited <- Array.append visited [|false|]
        let mutable components: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let v: int = _idx order ((n - i) - 1)
            if not (_idx visited (v)) then
                let mutable comp: int array = find_component (reversed) (v) (visited)
                components <- Array.append components [|comp|]
            i <- i + 1
        __ret <- components
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let test_graph_1: int array array = [|[|2; 3|]; [|0|]; [|1|]; [|4|]; [||]|]
        let test_graph_2: int array array = [|[|1; 2; 3|]; [|2|]; [|0|]; [|4|]; [|5|]; [|3|]|]
        printfn "%s" (_str (strongly_connected_components (test_graph_1)))
        printfn "%s" (_str (strongly_connected_components (test_graph_2)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
