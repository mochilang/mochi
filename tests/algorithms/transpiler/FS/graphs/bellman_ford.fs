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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Edge = {
    src: int
    dst: int
    weight: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: float = 1000000000.0
let rec list_to_string (arr: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            s <- s + (_str (_idx arr (i)))
            if i < ((Seq.length (arr)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
let rec check_negative_cycle (graph: Edge array) (distance: float array) (edge_count: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable graph = graph
    let mutable distance = distance
    let mutable edge_count = edge_count
    try
        let mutable j: int = 0
        while j < edge_count do
            let e: Edge = _idx graph (j)
            let u: int = e.src
            let v: int = e.dst
            let w: float = float (e.weight)
            if ((_idx distance (u)) < INF) && (((_idx distance (u)) + w) < (_idx distance (v))) then
                __ret <- true
                raise Return
            j <- j + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec bellman_ford (graph: Edge array) (vertex_count: int) (edge_count: int) (src: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable graph = graph
    let mutable vertex_count = vertex_count
    let mutable edge_count = edge_count
    let mutable src = src
    try
        let mutable distance: float array = [||]
        let mutable i: int = 0
        while i < vertex_count do
            distance <- Array.append distance [|INF|]
            i <- i + 1
        distance.[src] <- 0.0
        let mutable k: int = 0
        while k < (vertex_count - 1) do
            let mutable j: int = 0
            while j < edge_count do
                let e: Edge = _idx graph (j)
                let u: int = e.src
                let v: int = e.dst
                let w: float = float (e.weight)
                if ((_idx distance (u)) < INF) && (((_idx distance (u)) + w) < (_idx distance (v))) then
                    distance.[v] <- (_idx distance (u)) + w
                j <- j + 1
            k <- k + 1
        if check_negative_cycle (graph) (distance) (edge_count) then
            failwith ("Negative cycle found")
        __ret <- distance
        raise Return
        __ret
    with
        | Return -> __ret
let edges: Edge array = [|{ src = 2; dst = 1; weight = -10 }; { src = 3; dst = 2; weight = 3 }; { src = 0; dst = 3; weight = 5 }; { src = 0; dst = 1; weight = 4 }|]
let distances: float array = bellman_ford (edges) (4) (Seq.length (edges)) (0)
printfn "%s" (list_to_string (distances))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
