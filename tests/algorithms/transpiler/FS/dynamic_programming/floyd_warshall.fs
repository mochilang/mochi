// Generated 2025-08-07 15:46 +0700

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
type Graph = {
    n: int
    dp: int array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: int = 1000000000
let rec new_graph (n: int) =
    let mutable __ret : Graph = Unchecked.defaultof<Graph>
    let mutable n = n
    try
        let mutable dp: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|0|]
                else
                    row <- Array.append row [|INF|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        __ret <- { n = n; dp = dp }
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_edge (g: Graph) (u: int) (v: int) (w: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    let mutable w = w
    try
        let mutable dp: int array array = g.dp
        let mutable row: int array = _idx dp (u)
        row.[v] <- w
        dp.[u] <- row
        g <- { g with dp = dp }
        __ret
    with
        | Return -> __ret
let rec floyd_warshall (g: Graph) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        let mutable dp: int array array = g.dp
        let mutable k: int = 0
        while k < (g.n) do
            let mutable i: int = 0
            while i < (g.n) do
                let mutable j: int = 0
                while j < (g.n) do
                    let alt: int = (_idx (_idx dp (i)) (k)) + (_idx (_idx dp (k)) (j))
                    let mutable row: int array = _idx dp (i)
                    if alt < (_idx row (j)) then
                        row.[j] <- alt
                        dp.[i] <- row
                    j <- j + 1
                i <- i + 1
            k <- k + 1
        g <- { g with dp = dp }
        __ret
    with
        | Return -> __ret
let rec show_min (g: Graph) (u: int) (v: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable g = g
    let mutable u = u
    let mutable v = v
    try
        __ret <- _idx (_idx (g.dp) (u)) (v)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable graph: Graph = new_graph (5)
add_edge (graph) (0) (2) (9)
add_edge (graph) (0) (4) (10)
add_edge (graph) (1) (3) (5)
add_edge (graph) (2) (3) (7)
add_edge (graph) (3) (0) (10)
add_edge (graph) (3) (1) (2)
add_edge (graph) (3) (2) (1)
add_edge (graph) (3) (4) (6)
add_edge (graph) (4) (1) (3)
add_edge (graph) (4) (2) (4)
add_edge (graph) (4) (3) (9)
floyd_warshall (graph)
printfn "%s" (_str (show_min (graph) (1) (4)))
printfn "%s" (_str (show_min (graph) (0) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
