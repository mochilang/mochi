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
let rec depth_first_search (graph: int array array) (vertex: int) (visited: bool array) (rec_stk: bool array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable vertex = vertex
    let mutable visited = visited
    let mutable rec_stk = rec_stk
    try
        visited.[vertex] <- true
        rec_stk.[vertex] <- true
        for node in _idx graph (vertex) do
            if not (_idx visited (node)) then
                if depth_first_search (graph) (node) (visited) (rec_stk) then
                    __ret <- true
                    raise Return
            else
                if _idx rec_stk (node) then
                    __ret <- true
                    raise Return
        rec_stk.[vertex] <- false
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec check_cycle (graph: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    try
        let n: int = Seq.length (graph)
        let mutable visited: bool array = [||]
        let mutable rec_stk: bool array = [||]
        let mutable i: int = 0
        while i < n do
            visited <- Array.append visited [|false|]
            rec_stk <- Array.append rec_stk [|false|]
            i <- i + 1
        i <- 0
        while i < n do
            if not (_idx visited (i)) then
                if depth_first_search (graph) (i) (visited) (rec_stk) then
                    __ret <- true
                    raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_bool (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        if b then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        __ret
    with
        | Return -> __ret
let g1 = [|[||]; [|0; 3|]; [|0; 4|]; [|5|]; [|5|]; [||]|]
print_bool (check_cycle (unbox<int array array> g1))
let g2: int array array = [|[|1; 2|]; [|2|]; [|0; 3|]; [|3|]|]
print_bool (check_cycle (g2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
