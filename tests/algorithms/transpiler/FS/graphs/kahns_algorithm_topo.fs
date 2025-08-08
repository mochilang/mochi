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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
open System.Collections.Generic

let rec topological_sort (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable graph = graph
    try
        let mutable indegree: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (graph)) do
            indegree <- Array.append indegree [|0|]
            i <- i + 1
        for edges in Seq.map snd (graph) do
            let mutable j: int = 0
            while j < (Seq.length (edges)) do
                let v = _idx edges (j)
                indegree.[v] <- (_idx indegree (v)) + 1
                j <- j + 1
        let mutable queue: int array = [||]
        i <- 0
        while i < (Seq.length (indegree)) do
            if (_idx indegree (i)) = 0 then
                queue <- Array.append queue [|i|]
            i <- i + 1
        let mutable order: int array = [||]
        let mutable head: int = 0
        let mutable processed: int = 0
        while head < (Seq.length (queue)) do
            let v: int = _idx queue (head)
            head <- head + 1
            processed <- processed + 1
            order <- Array.append order [|v|]
            let neighbors: int array = _dictGet graph (v)
            let mutable k: int = 0
            while k < (Seq.length (neighbors)) do
                let nb: int = _idx neighbors (k)
                indegree.[nb] <- (_idx indegree (nb)) - 1
                if (_idx indegree (nb)) = 0 then
                    queue <- Array.append queue [|nb|]
                k <- k + 1
        if processed <> (Seq.length (graph)) then
            __ret <- unbox<int array> null
            raise Return
        __ret <- order
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 2|]); (1, [|3|]); (2, [|3|]); (3, [|4; 5|]); (4, Array.empty<int>); (5, Array.empty<int>)]
        printfn "%s" (_repr (topological_sort (graph)))
        let cyclic: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1|]); (1, [|2|]); (2, [|0|])]
        printfn "%s" (_repr (topological_sort (cyclic)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
