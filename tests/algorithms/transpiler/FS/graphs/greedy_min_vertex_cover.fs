// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec remove_value (lst: int array) (``val``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable lst = lst
    let mutable ``val`` = ``val``
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) <> ``val`` then
                res <- Array.append res [|(_idx lst (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec greedy_min_vertex_cover (graph: System.Collections.Generic.IDictionary<int, int array>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    try
        let mutable g: System.Collections.Generic.IDictionary<int, int array> = graph
        let mutable cover: int array = [||]
        try
            while true do
                try
                    let mutable max_v: int = 0
                    let mutable max_deg: int = 0
                    for v in g.Keys do
                        let key: int = int v
                        let deg: int = Seq.length (_dictGet g (key))
                        if deg > max_deg then
                            max_deg <- deg
                            max_v <- key
                    if max_deg = 0 then
                        raise Break
                    cover <- Array.append cover [|max_v|]
                    let neighbors: int array = _dictGet g (max_v)
                    let mutable i: int = 0
                    while i < (Seq.length (neighbors)) do
                        let n: int = _idx neighbors (i)
                        g.[n] <- remove_value (_dictGet g (n)) (max_v)
                        i <- i + 1
                    g.[max_v] <- [||]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- cover
        raise Return
        __ret
    with
        | Return -> __ret
let mutable graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(0, [|1; 3|]); (1, [|0; 3|]); (2, [|0; 3; 4|]); (3, [|0; 1; 2|]); (4, [|2; 3|])]
printfn "%s" (_repr (greedy_min_vertex_cover (graph)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
