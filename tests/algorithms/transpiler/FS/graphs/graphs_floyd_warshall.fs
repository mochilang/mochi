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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: float = 1000000000.0
let rec floyd_warshall (graph: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable graph = graph
    try
        let v: int = Seq.length (graph)
        let mutable dist: float array array = [||]
        let mutable i: int = 0
        while i < v do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < v do
                row <- Array.append row [|(_idx (_idx graph (i)) (j))|]
                j <- j + 1
            dist <- Array.append dist [|row|]
            i <- i + 1
        let mutable k: int = 0
        while k < v do
            let mutable i: int = 0
            while i < v do
                let mutable j: int = 0
                while j < v do
                    if (((_idx (_idx dist (i)) (k)) < INF) && ((_idx (_idx dist (k)) (j)) < INF)) && (((_idx (_idx dist (i)) (k)) + (_idx (_idx dist (k)) (j))) < (_idx (_idx dist (i)) (j))) then
                        dist.[i].[j] <- (_idx (_idx dist (i)) (k)) + (_idx (_idx dist (k)) (j))
                    j <- j + 1
                i <- i + 1
            k <- k + 1
        __ret <- dist
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_dist (dist: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable dist = dist
    try
        printfn "%s" ("\nThe shortest path matrix using Floyd Warshall algorithm\n")
        let mutable i: int = 0
        while i < (Seq.length (dist)) do
            let mutable j: int = 0
            let mutable line: string = ""
            while j < (Seq.length (_idx dist (i))) do
                if (_idx (_idx dist (i)) (j)) >= (INF / 2.0) then
                    line <- line + "INF\t"
                else
                    line <- (line + (_str (int (_idx (_idx dist (i)) (j))))) + "\t"
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let graph: float array array = [|[|0.0; 5.0; INF; 10.0|]; [|INF; 0.0; 3.0; INF|]; [|INF; INF; 0.0; 1.0|]; [|INF; INF; INF; 0.0|]|]
let result: float array array = floyd_warshall (graph)
print_dist (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
