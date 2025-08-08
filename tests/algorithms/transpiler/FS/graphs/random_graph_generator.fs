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
open System.Collections.Generic

let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and complete_graph (vertices_number: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int array>>
    let mutable vertices_number = vertices_number
    try
        let mutable graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
        let mutable i: int = 0
        while i < vertices_number do
            let mutable neighbors: int array = [||]
            let mutable j: int = 0
            while j < vertices_number do
                if j <> i then
                    neighbors <- Array.append neighbors [|j|]
                j <- j + 1
            graph.[i] <- neighbors
            i <- i + 1
        __ret <- graph
        raise Return
        __ret
    with
        | Return -> __ret
and random_graph (vertices_number: int) (probability: float) (directed: bool) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int array>>
    let mutable vertices_number = vertices_number
    let mutable probability = probability
    let mutable directed = directed
    try
        let mutable graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
        let mutable i: int = 0
        while i < vertices_number do
            graph.[i] <- [||]
            i <- i + 1
        if probability >= 1.0 then
            __ret <- complete_graph (vertices_number)
            raise Return
        if probability <= 0.0 then
            __ret <- graph
            raise Return
        i <- 0
        while i < vertices_number do
            let mutable j: int = i + 1
            while j < vertices_number do
                if (random()) < probability then
                    graph.[i] <- Array.append (_dictGet graph (i)) [|j|]
                    if not directed then
                        graph.[j] <- Array.append (_dictGet graph (j)) [|i|]
                j <- j + 1
            i <- i + 1
        __ret <- graph
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        _seed <- 1
        let g1: System.Collections.Generic.IDictionary<int, int array> = random_graph (4) (0.5) (false)
        printfn "%A" (g1)
        _seed <- 1
        let g2: System.Collections.Generic.IDictionary<int, int array> = random_graph (4) (0.5) (true)
        printfn "%A" (g2)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
