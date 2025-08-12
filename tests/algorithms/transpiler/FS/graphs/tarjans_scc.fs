// Generated 2025-08-12 08:38 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
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
        let mutable stack: int array = Array.empty<int>
        let mutable on_stack: bool array = Array.empty<bool>
        let mutable index_of: int array = Array.empty<int>
        let mutable lowlink_of: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            on_stack <- Array.append on_stack [|false|]
            index_of <- Array.append index_of [|(0 - 1)|]
            lowlink_of <- Array.append lowlink_of [|(0 - 1)|]
            i <- i + 1
        let mutable components: int array array = Array.empty<int array>
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
                for w in _idx g (int v) do
                    if (_idx index_of (int w)) = (0 - 1) then
                        current_index <- strong_connect (w) (current_index)
                        if (_idx lowlink_of (int w)) < (_idx lowlink_of (int v)) then
                            lowlink_of.[v] <- _idx lowlink_of (int w)
                    else
                        if _idx on_stack (int w) then
                            if (_idx lowlink_of (int w)) < (_idx lowlink_of (int v)) then
                                lowlink_of.[v] <- _idx lowlink_of (int w)
                if (_idx lowlink_of (int v)) = (_idx index_of (int v)) then
                    let mutable component: int array = Array.empty<int>
                    let mutable w: int = _idx stack (int ((Seq.length (stack)) - 1))
                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                    on_stack.[w] <- false
                    component <- Array.append component [|w|]
                    while w <> v do
                        w <- _idx stack (int ((Seq.length (stack)) - 1))
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
            if (_idx index_of (int v)) = (0 - 1) then
                ignore (strong_connect (v) (0))
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
        let mutable g: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            g <- Array.append g [|[||]|]
            i <- i + 1
        for e in edges do
            let u: int = _idx e (int 0)
            let mutable v: int = _idx e (int 1)
            g.[u] <- Array.append (_idx g (int u)) [|v|]
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
        let source: int array = unbox<int array> [|0; 0; 1; 2; 3; 3; 4; 4; 6|]
        let target: int array = unbox<int array> [|1; 3; 2; 0; 1; 4; 5; 6; 5|]
        let mutable edges: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (source)) do
            edges <- Array.append edges [|[|_idx source (int i); _idx target (int i)|]|]
            i <- i + 1
        let mutable g: int array array = create_graph (n_vertices) (edges)
        ignore (printfn "%s" (_str (tarjan (g))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
