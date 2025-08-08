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
let rec dfs (u: int) (graph: int array array) (visit: bool array) (stack: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable u = u
    let mutable graph = graph
    let mutable visit = visit
    let mutable stack = stack
    try
        if _idx visit (u) then
            __ret <- stack
            raise Return
        visit.[u] <- true
        for v in _idx graph (u) do
            stack <- dfs (v) (graph) (visit) (stack)
        stack <- Array.append stack [|u|]
        __ret <- stack
        raise Return
        __ret
    with
        | Return -> __ret
and dfs2 (u: int) (reversed_graph: int array array) (visit: bool array) (component: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable u = u
    let mutable reversed_graph = reversed_graph
    let mutable visit = visit
    let mutable component = component
    try
        if _idx visit (u) then
            __ret <- component
            raise Return
        visit.[u] <- true
        component <- Array.append component [|u|]
        for v in _idx reversed_graph (u) do
            component <- dfs2 (v) (reversed_graph) (visit) (component)
        __ret <- component
        raise Return
        __ret
    with
        | Return -> __ret
and kosaraju (graph: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable reversed_graph: int array array = [||]
        let mutable i: int = 0
        while i < n do
            reversed_graph <- Array.append reversed_graph [|[||]|]
            i <- i + 1
        i <- 0
        while i < n do
            for v in _idx graph (i) do
                reversed_graph.[v] <- Array.append (_idx reversed_graph (v)) [|i|]
            i <- i + 1
        let mutable visit: bool array = [||]
        i <- 0
        while i < n do
            visit <- Array.append visit [|false|]
            i <- i + 1
        let mutable stack: int array = [||]
        i <- 0
        while i < n do
            if (_idx visit (i)) = false then
                stack <- dfs (i) (graph) (visit) (stack)
            i <- i + 1
        i <- 0
        while i < n do
            visit.[i] <- false
            i <- i + 1
        let mutable scc: int array array = [||]
        let mutable idx: int = (Seq.length (stack)) - 1
        while idx >= 0 do
            let node: int = _idx stack (idx)
            if (_idx visit (node)) = false then
                let mutable component: int array = [||]
                component <- dfs2 (node) (reversed_graph) (visit) (component)
                scc <- Array.append scc [|component|]
            idx <- idx - 1
        __ret <- scc
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let graph: int array array = [|[|1|]; [|2|]; [|0; 3|]; [|4|]; [||]|]
        let comps: int array array = kosaraju (graph)
        let mutable i: int = 0
        while i < (Seq.length (comps)) do
            printfn "%s" (_repr (_idx comps (i)))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
