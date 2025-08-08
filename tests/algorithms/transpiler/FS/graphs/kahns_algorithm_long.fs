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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec longest_distance (graph: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable indegree: int array = [||]
        let mutable i: int = 0
        while i < n do
            indegree <- Array.append indegree [|0|]
            i <- i + 1
        let mutable long_dist: int array = [||]
        let mutable j: int = 0
        while j < n do
            long_dist <- Array.append long_dist [|1|]
            j <- j + 1
        let mutable u: int = 0
        while u < n do
            for v in _idx graph (u) do
                indegree.[v] <- (_idx indegree (v)) + 1
            u <- u + 1
        let mutable queue: int array = [||]
        let mutable head: int = 0
        let mutable k: int = 0
        while k < n do
            if (_idx indegree (k)) = 0 then
                queue <- Array.append queue [|k|]
            k <- k + 1
        while head < (Seq.length (queue)) do
            let vertex: int = _idx queue (head)
            head <- head + 1
            for x in _idx graph (vertex) do
                indegree.[x] <- (_idx indegree (x)) - 1
                let new_dist: int = (_idx long_dist (vertex)) + 1
                if new_dist > (_idx long_dist (x)) then
                    long_dist.[x] <- new_dist
                if (_idx indegree (x)) = 0 then
                    queue <- Array.append queue [|x|]
        let mutable max_len: int = _idx long_dist (0)
        let mutable m: int = 1
        while m < n do
            if (_idx long_dist (m)) > max_len then
                max_len <- _idx long_dist (m)
            m <- m + 1
        __ret <- max_len
        raise Return
        __ret
    with
        | Return -> __ret
let graph: int array array = [|[|2; 3; 4|]; [|2; 7|]; [|5|]; [|5; 7|]; [|7|]; [|6|]; [|7|]; [||]|]
printfn "%d" (longest_distance (graph))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
