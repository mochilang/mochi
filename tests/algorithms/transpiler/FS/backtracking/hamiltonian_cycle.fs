// Generated 2025-08-06 21:04 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec valid_connection (graph: int array array) (next_ver: int) (curr_ind: int) (path: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable next_ver = next_ver
    let mutable curr_ind = curr_ind
    let mutable path = path
    try
        if (_idx (_idx graph (_idx path (curr_ind - 1))) (next_ver)) = 0 then
            __ret <- false
            raise Return
        for v in path do
            if v = next_ver then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and util_hamilton_cycle (graph: int array array) (path: int array) (curr_ind: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable path = path
    let mutable curr_ind = curr_ind
    try
        if curr_ind = (Seq.length (graph)) then
            __ret <- (_idx (_idx graph (_idx path (curr_ind - 1))) (_idx path (0))) = 1
            raise Return
        let mutable next_ver: int = 0
        while next_ver < (Seq.length (graph)) do
            if valid_connection (graph) (next_ver) (curr_ind) (path) then
                path <- _arrset path curr_ind next_ver
                if util_hamilton_cycle (graph) (path) (curr_ind + 1) then
                    __ret <- true
                    raise Return
                path <- _arrset path curr_ind -1
            next_ver <- next_ver + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and hamilton_cycle (graph: int array array) (start_index: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable start_index = start_index
    try
        let mutable path: int array = Array.empty<int>
        let mutable i: int = 0
        while i < ((Seq.length (graph)) + 1) do
            path <- _arrset path i -1
            i <- i + 1
        path <- _arrset path 0 start_index
        let mutable last: int = (Seq.length (path)) - 1
        path <- _arrset path last start_index
        if util_hamilton_cycle (graph) (path) (1) then
            __ret <- path
            raise Return
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
