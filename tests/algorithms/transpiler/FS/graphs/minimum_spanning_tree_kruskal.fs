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
let rec sort_edges (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable edges = edges
    try
        let mutable es: int array array = edges
        let mutable i: int = 0
        while i < (Seq.length (es)) do
            let mutable j: int = 0
            while j < (((Seq.length (es)) - i) - 1) do
                if (_idx (_idx es (j)) (2)) > (_idx (_idx es (j + 1)) (2)) then
                    let temp: int array = _idx es (j)
                    es.[j] <- _idx es (j + 1)
                    es.[j + 1] <- temp
                j <- j + 1
            i <- i + 1
        __ret <- es
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_parent (parent: int array) (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable parent = parent
    let mutable i = i
    try
        if (_idx parent (i)) <> i then
            parent.[i] <- find_parent (parent) (_idx parent (i))
        __ret <- _idx parent (i)
        raise Return
        __ret
    with
        | Return -> __ret
let rec kruskal (num_nodes: int) (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable num_nodes = num_nodes
    let mutable edges = edges
    try
        let mutable es: int array array = sort_edges (edges)
        let mutable parent: int array = [||]
        let mutable i: int = 0
        while i < num_nodes do
            parent <- Array.append parent [|i|]
            i <- i + 1
        let mutable mst: int array array = [||]
        let mutable idx: int = 0
        while idx < (Seq.length (es)) do
            let e: int array = _idx es (idx)
            let pa: int = find_parent (parent) (_idx e (0))
            let pb: int = find_parent (parent) (_idx e (1))
            if pa <> pb then
                mst <- Array.append mst [|e|]
                parent.[pa] <- pb
            idx <- idx + 1
        __ret <- mst
        raise Return
        __ret
    with
        | Return -> __ret
let rec edges_to_string (es: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable es = es
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (es)) do
            let e: int array = _idx es (i)
            s <- ((((((s + "(") + (_str (_idx e (0)))) + ", ") + (_str (_idx e (1)))) + ", ") + (_str (_idx e (2)))) + ")"
            if i < ((Seq.length (es)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let edges1: int array array = [|[|0; 1; 3|]; [|1; 2; 5|]; [|2; 3; 1|]|]
printfn "%s" (edges_to_string (kruskal (4) (edges1)))
let edges2: int array array = [|[|0; 1; 3|]; [|1; 2; 5|]; [|2; 3; 1|]; [|0; 2; 1|]; [|0; 3; 2|]|]
printfn "%s" (edges_to_string (kruskal (4) (edges2)))
let edges3: int array array = [|[|0; 1; 3|]; [|1; 2; 5|]; [|2; 3; 1|]; [|0; 2; 1|]; [|0; 3; 2|]; [|2; 1; 1|]|]
printfn "%s" (edges_to_string (kruskal (4) (edges3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
