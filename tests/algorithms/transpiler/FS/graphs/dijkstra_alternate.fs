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
let rec minimum_distance (distances: int array) (visited: bool array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable distances = distances
    let mutable visited = visited
    try
        let mutable minimum: int = 10000000
        let mutable min_index: int = 0
        let mutable vertex: int = 0
        while vertex < (Seq.length (distances)) do
            if ((_idx distances (vertex)) < minimum) && ((_idx visited (vertex)) = false) then
                minimum <- _idx distances (vertex)
                min_index <- vertex
            vertex <- vertex + 1
        __ret <- min_index
        raise Return
        __ret
    with
        | Return -> __ret
let rec dijkstra (graph: int array array) (source: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable source = source
    try
        let vertices: int = Seq.length (graph)
        let mutable distances: int array = Array.empty<int>
        let mutable i: int = 0
        while i < vertices do
            distances <- Array.append distances [|10000000|]
            i <- i + 1
        distances <- _arrset distances (source) (0)
        let mutable visited: bool array = Array.empty<bool>
        i <- 0
        while i < vertices do
            visited <- Array.append visited [|false|]
            i <- i + 1
        let mutable count: int = 0
        while count < vertices do
            let u: int = minimum_distance (distances) (visited)
            visited.[u] <- true
            let mutable v: int = 0
            while v < vertices do
                if (((_idx (_idx graph (u)) (v)) > 0) && ((_idx visited (v)) = false)) && ((_idx distances (v)) > ((_idx distances (u)) + (_idx (_idx graph (u)) (v)))) then
                    distances <- _arrset distances (v) ((_idx distances (u)) + (_idx (_idx graph (u)) (v)))
                v <- v + 1
            count <- count + 1
        __ret <- distances
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_solution (distances: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable distances = distances
    try
        printfn "%s" ("Vertex \t Distance from Source")
        let mutable v: int = 0
        while v < (Seq.length (distances)) do
            printfn "%s" (((_str (v)) + "\t\t") + (_str (_idx distances (v))))
            v <- v + 1
        __ret
    with
        | Return -> __ret
let graph: int array array = [|[|0; 4; 0; 0; 0; 0; 0; 8; 0|]; [|4; 0; 8; 0; 0; 0; 0; 11; 0|]; [|0; 8; 0; 7; 0; 4; 0; 0; 2|]; [|0; 0; 7; 0; 9; 14; 0; 0; 0|]; [|0; 0; 0; 9; 0; 10; 0; 0; 0|]; [|0; 0; 4; 14; 10; 0; 2; 0; 0|]; [|0; 0; 0; 0; 0; 2; 0; 1; 6|]; [|8; 11; 0; 0; 0; 0; 1; 0; 7|]; [|0; 0; 2; 0; 0; 0; 6; 7; 0|]|]
let mutable distances: int array = dijkstra (graph) (0)
print_solution (distances)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
