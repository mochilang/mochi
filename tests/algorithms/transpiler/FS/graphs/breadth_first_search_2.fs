// Generated 2025-08-07 16:27 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec join (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            s <- s + (_idx xs (i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec breadth_first_search (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable graph = graph
    let mutable start = start
    try
        let mutable explored: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        explored.[start] <- true
        let mutable result: string array = [|start|]
        let mutable queue: string array = [|start|]
        while (Seq.length (queue)) > 0 do
            let v: string = _idx queue (0)
            queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
            let children: string array = _dictGet graph ((string (v)))
            let mutable i: int = 0
            while i < (Seq.length (children)) do
                let w: string = _idx children (i)
                if not (explored.ContainsKey(w)) then
                    explored.[w] <- true
                    result <- Array.append result [|w|]
                    queue <- Array.append queue [|w|]
                i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec breadth_first_search_with_deque (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable graph = graph
    let mutable start = start
    try
        let mutable visited: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        visited.[start] <- true
        let mutable result: string array = [|start|]
        let mutable queue: string array = [|start|]
        let mutable head: int = 0
        while head < (Seq.length (queue)) do
            let v: string = _idx queue (head)
            head <- head + 1
            let children: string array = _dictGet graph ((string (v)))
            let mutable i: int = 0
            while i < (Seq.length (children)) do
                let child: string = _idx children (i)
                if not (visited.ContainsKey(child)) then
                    visited.[child] <- true
                    result <- Array.append result [|child|]
                    queue <- Array.append queue [|child|]
                i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let G: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("A", [|"B"; "C"|]); ("B", [|"A"; "D"; "E"|]); ("C", [|"A"; "F"|]); ("D", [|"B"|]); ("E", [|"B"; "F"|]); ("F", [|"C"; "E"|])]
printfn "%s" (join (breadth_first_search (G) ("A")))
printfn "%s" (join (breadth_first_search_with_deque (G) ("A")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
