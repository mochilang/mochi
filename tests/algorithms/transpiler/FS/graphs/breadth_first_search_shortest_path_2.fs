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

let rec contains (xs: string array) (x: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_key (m: System.Collections.Generic.IDictionary<string, string array>) (key: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    let mutable key = key
    try
        for k in m.Keys do
            if k = key then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec bfs_shortest_path (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) (goal: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable graph = graph
    let mutable start = start
    let mutable goal = goal
    try
        let mutable explored: string array = [||]
        let mutable queue: string array array = [|[|start|]|]
        if start = goal then
            __ret <- unbox<string array> [|start|]
            raise Return
        while (Seq.length (queue)) > 0 do
            let path: string array = _idx queue (0)
            queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
            let node: string = _idx path ((Seq.length (path)) - 1)
            if not (contains (explored) (node)) then
                let neighbours: string array = _dictGet graph ((string (node)))
                let mutable i: int = 0
                while i < (Seq.length (neighbours)) do
                    let neighbour: string = _idx neighbours (i)
                    let mutable new_path: string array = path
                    new_path <- Array.append new_path [|neighbour|]
                    queue <- Array.append queue [|new_path|]
                    if neighbour = goal then
                        __ret <- new_path
                        raise Return
                    i <- i + 1
                explored <- Array.append explored [|node|]
        __ret <- Array.empty<string>
        raise Return
        __ret
    with
        | Return -> __ret
let rec bfs_shortest_path_distance (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable graph = graph
    let mutable start = start
    let mutable target = target
    try
        if ((contains_key (graph) (start)) = false) || ((contains_key (graph) (target)) = false) then
            __ret <- -1
            raise Return
        if start = target then
            __ret <- 0
            raise Return
        let mutable queue: string array = [|start|]
        let mutable visited: string array = [|start|]
        let mutable dist: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        dist.[start] <- 0
        dist.[target] <- -1
        while (Seq.length (queue)) > 0 do
            let node: string = _idx queue (0)
            queue <- Array.sub queue 1 ((Seq.length (queue)) - 1)
            if node = target then
                if ((_dictGet dist ((string (target)))) = (-1)) || ((_dictGet dist ((string (node)))) < (_dictGet dist ((string (target))))) then
                    dist.[target] <- _dictGet dist ((string (node)))
            let adj: string array = _dictGet graph ((string (node)))
            let mutable i: int = 0
            while i < (Seq.length (adj)) do
                let next: string = _idx adj (i)
                if not (contains (visited) (next)) then
                    visited <- Array.append visited [|next|]
                    queue <- Array.append queue [|next|]
                    dist.[next] <- (_dictGet dist ((string (node)))) + 1
                i <- i + 1
        __ret <- _dictGet dist ((string (target)))
        raise Return
        __ret
    with
        | Return -> __ret
let demo_graph: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("A", [|"B"; "C"; "E"|]); ("B", [|"A"; "D"; "E"|]); ("C", [|"A"; "F"; "G"|]); ("D", [|"B"|]); ("E", [|"A"; "B"; "D"|]); ("F", [|"C"|]); ("G", [|"C"|])]
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
