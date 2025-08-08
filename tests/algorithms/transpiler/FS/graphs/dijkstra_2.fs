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
let INF: float = 1000000000.0
let rec print_dist (dist: float array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable dist = dist
    try
        printfn "%s" ("Vertex Distance")
        let mutable i: int = 0
        while i < (Seq.length (dist)) do
            if (_idx dist (i)) >= INF then
                printfn "%s" (String.concat " " ([|sprintf "%d" (i); sprintf "%s" ("\tINF")|]))
            else
                printfn "%s" (String.concat " " ([|sprintf "%d" (i); sprintf "%s" ("\t"); sprintf "%d" (int (_idx dist (i)))|]))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and min_dist (mdist: float array) (vset: bool array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable mdist = mdist
    let mutable vset = vset
    try
        let mutable min_val: float = INF
        let mutable min_ind: int = -1
        let mutable i: int = 0
        while i < (Seq.length (mdist)) do
            if (not (_idx vset (i))) && ((_idx mdist (i)) < min_val) then
                min_val <- _idx mdist (i)
                min_ind <- i
            i <- i + 1
        __ret <- min_ind
        raise Return
        __ret
    with
        | Return -> __ret
and dijkstra (graph: float array array) (src: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable graph = graph
    let mutable src = src
    try
        let v: int = Seq.length (graph)
        let mutable mdist: float array = [||]
        let mutable vset: bool array = [||]
        let mutable i: int = 0
        while i < v do
            mdist <- Array.append mdist [|INF|]
            vset <- Array.append vset [|false|]
            i <- i + 1
        mdist.[src] <- 0.0
        let mutable count: int = 0
        while count < (v - 1) do
            let u: int = min_dist (mdist) (vset)
            vset.[u] <- true
            let mutable i: int = 0
            while i < v do
                let alt: float = (_idx mdist (u)) + (_idx (_idx graph (u)) (i))
                if ((not (_idx vset (i))) && ((_idx (_idx graph (u)) (i)) < INF)) && (alt < (_idx mdist (i))) then
                    mdist.[i] <- alt
                i <- i + 1
            count <- count + 1
        __ret <- mdist
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let graph: float array array = [|[|0.0; 10.0; INF; INF; 5.0|]; [|INF; 0.0; 1.0; INF; 2.0|]; [|INF; INF; 0.0; 4.0; INF|]; [|INF; INF; 6.0; 0.0; INF|]; [|INF; 3.0; 9.0; 2.0; 0.0|]|]
        let dist: float array = dijkstra (graph) (0)
        print_dist (dist)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
