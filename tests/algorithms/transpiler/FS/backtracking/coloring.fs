// Generated 2025-08-06 20:48 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec valid_coloring (neighbours: int array) (colored_vertices: int array) (color: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable neighbours = neighbours
    let mutable colored_vertices = colored_vertices
    let mutable color = color
    try
        let mutable i: int = 0
        while i < (Seq.length(neighbours)) do
            if ((_idx neighbours (i)) = 1) && ((_idx colored_vertices (i)) = color) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and util_color (graph: int array array) (max_colors: int) (colored_vertices: int array) (index: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable max_colors = max_colors
    let mutable colored_vertices = colored_vertices
    let mutable index = index
    try
        if index = (Seq.length(graph)) then
            __ret <- true
            raise Return
        let mutable c: int = 0
        while c < max_colors do
            if valid_coloring (_idx graph (index)) (colored_vertices) (c) then
                colored_vertices.[index] <- c
                if util_color (graph) (max_colors) (colored_vertices) (index + 1) then
                    __ret <- true
                    raise Return
                colored_vertices.[index] <- -1
            c <- c + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and color (graph: int array array) (max_colors: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable max_colors = max_colors
    try
        let mutable colored_vertices: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length(graph)) do
            colored_vertices <- Array.append colored_vertices [|-1|]
            i <- i + 1
        if util_color (graph) (max_colors) (colored_vertices) (0) then
            __ret <- colored_vertices
            raise Return
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
let graph: int array array = [|[|0; 1; 0; 0; 0|]; [|1; 0; 1; 0; 1|]; [|0; 1; 0; 1; 0|]; [|0; 1; 1; 0; 0|]; [|0; 1; 0; 0; 0|]|]
printfn "%s" (_repr (color (graph) (3)))
printfn "%s" ("\n")
printfn "%d" (Seq.length(color (graph) (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
