// Generated 2025-08-07 16:27 +0700

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
let rec dfs_skip (graph: int array array) (visited: bool array) (skip: int) (at: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable graph = graph
    let mutable visited = visited
    let mutable skip = skip
    let mutable at = at
    try
        visited.[at] <- true
        let mutable count: int = 1
        try
            for ``to`` in _idx graph (at) do
                try
                    if ``to`` = skip then
                        raise Continue
                    if (_idx visited (``to``)) = false then
                        count <- count + (dfs_skip (graph) (visited) (skip) (``to``))
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and articulation_points (graph: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable result: int array = [||]
        let mutable v: int = 0
        while v < n do
            let mutable visited: bool array = [||]
            let mutable i: int = 0
            while i < n do
                visited <- Array.append visited [|false|]
                i <- i + 1
            let mutable start: int = 0
            while (start = v) && (start < n) do
                start <- start + 1
            let reach: int = dfs_skip (graph) (visited) (v) (start)
            if reach < (n - 1) then
                result <- Array.append result [|v|]
                printfn "%d" (v)
            v <- v + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let graph: int array array = [|[|1; 2|]; [|0; 2|]; [|0; 1; 3; 5|]; [|2; 4|]; [|3|]; [|2; 6; 8|]; [|5; 7|]; [|6; 8|]; [|5; 7|]|]
        articulation_points (graph)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
