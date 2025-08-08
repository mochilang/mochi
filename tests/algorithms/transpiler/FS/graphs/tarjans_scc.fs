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
let rec tarjan (g: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable g = g
    try
        let n: int = Seq.length (g)
        let mutable stack: int array = [||]
        let mutable on_stack: bool array = [||]
        let mutable index_of: int array = [||]
        let mutable lowlink_of: int array = [||]
        let mutable i: int = 0
        while i < n do
            on_stack <- Array.append on_stack [|false|]
            index_of <- Array.append index_of [|(0 - 1)|]
            lowlink_of <- Array.append lowlink_of [|(0 - 1)|]
            i <- i + 1
        let mutable components: int array array = [||]
        let rec strong_connect (v: int) (index: int) =
            let mutable __ret : int = Unchecked.defaultof<int>
            let mutable v = v
            let mutable index = index
            try
                index_of.[v] <- index
                lowlink_of.[v] <- index
                let mutable current_index: int = index + 1
                stack <- Array.append stack [|v|]
                on_stack.[v] <- true
                for w in _idx g (v) do
                    if (_idx index_of (w)) = (0 - 1) then
                        current_index <- strong_connect (w) (current_index)
                        if (_idx lowlink_of (w)) < (_idx lowlink_of (v)) then
                            lowlink_of.[v] <- _idx lowlink_of (w)
                    else
                        if _idx on_stack (w) then
                            if (_idx lowlink_of (w)) < (_idx lowlink_of (v)) then
                                lowlink_of.[v] <- _idx lowlink_of (w)
                if (_idx lowlink_of (v)) = (_idx index_of (v)) then
                    let mutable component: int array = [||]
                    let mutable w: int = _idx stack ((Seq.length (stack)) - 1)
                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                    on_stack.[w] <- false
                    component <- Array.append component [|w|]
                    while w <> v do
                        w <- _idx stack ((Seq.length (stack)) - 1)
                        stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                        on_stack.[w] <- false
                        component <- Array.append component [|w|]
                    components <- Array.append components [|component|]
                __ret <- current_index
                raise Return
                __ret
            with
                | Return -> __ret
        let mutable v: int = 0
        while v < n do
            if (_idx index_of (v)) = (0 - 1) then
                strong_connect (v) (0)
            v <- v + 1
        __ret <- components
        raise Return
        __ret
    with
        | Return -> __ret
and create_graph (n: int) (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    let mutable edges = edges
    try
        let mutable g: int array array = [||]
        let mutable i: int = 0
        while i < n do
            g <- Array.append g [|[||]|]
            i <- i + 1
        for e in edges do
            let u: int = _idx e (0)
            let mutable v: int = _idx e (1)
            g.[u] <- Array.append (_idx g (u)) [|v|]
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let n_vertices: int = 7
        let source: int array = [|0; 0; 1; 2; 3; 3; 4; 4; 6|]
        let target: int array = [|1; 3; 2; 0; 1; 4; 5; 6; 5|]
        let mutable edges: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (source)) do
            edges <- Array.append edges [|[|_idx source (i); _idx target (i)|]|]
            i <- i + 1
        let mutable g: int array array = create_graph (n_vertices) (edges)
        printfn "%s" (_str (tarjan (g)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
