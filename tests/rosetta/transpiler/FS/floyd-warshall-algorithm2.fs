// Generated 2025-08-01 15:22 +0700

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
type FWResult = {
    dist: int array array
    next: int array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: int = 1000000
let rec floydWarshall (graph: int array array) =
    let mutable __ret : FWResult = Unchecked.defaultof<FWResult>
    let mutable graph = graph
    try
        let n: int = Seq.length graph
        let mutable dist: int array array = [||]
        let mutable next: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable drow: int array = [||]
            let mutable nrow: int array = [||]
            let mutable j: int = 0
            while j < n do
                drow <- Array.append drow [|(graph.[i]).[j]|]
                if (((graph.[i]).[j]) < INF) && (i <> j) then
                    nrow <- Array.append nrow [|j|]
                else
                    nrow <- Array.append nrow [|-1|]
                j <- j + 1
            dist <- Array.append dist [|drow|]
            next <- Array.append next [|nrow|]
            i <- i + 1
        let mutable k: int = 0
        while k < n do
            let mutable i: int = 0
            while i < n do
                let mutable j: int = 0
                while j < n do
                    if (((dist.[i]).[k]) < INF) && (((dist.[k]).[j]) < INF) then
                        let alt: int = ((dist.[i]).[k]) + ((dist.[k]).[j])
                        if alt < ((dist.[i]).[j]) then
                            (dist.[i]).[j] <- alt
                            (next.[i]).[j] <- (next.[i]).[k]
                    j <- j + 1
                i <- i + 1
            k <- k + 1
        __ret <- { dist = dist; next = next }
        raise Return
        __ret
    with
        | Return -> __ret
let rec path (u: int) (v: int) (next: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable u = u
    let mutable v = v
    let mutable next = next
    try
        if ((next.[u]).[v]) < 0 then
            __ret <- Array.empty<int>
            raise Return
        let mutable p: int array = [|u|]
        let mutable x: int = u
        while x <> v do
            x <- (next.[x]).[v]
            p <- Array.append p [|x|]
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec pathStr (p: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length p) do
            s <- s + (string ((p.[i]) + 1))
            if i < ((Seq.length p) - 1) then
                s <- s + " -> "
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let n: int = 4
let mutable g: int array array = [||]
for i in 0 .. (n - 1) do
    let mutable row: int array = [||]
    for j in 0 .. (n - 1) do
        if i = j then
            row <- Array.append row [|0|]
        else
            row <- Array.append row [|INF|]
    g <- Array.append g [|row|]
(g.[0]).[2] <- -2
(g.[2]).[3] <- 2
(g.[3]).[1] <- -1
(g.[1]).[0] <- 4
(g.[1]).[2] <- 3
let res: FWResult = floydWarshall g
printfn "%s" "pair\tdist\tpath"
let mutable i: int = 0
while i < n do
    let mutable j: int = 0
    while j < n do
        if i <> j then
            let mutable p: int array = path i j (res.next)
            printfn "%s" (((((((string (i + 1)) + " -> ") + (string (j + 1))) + "\t") + (string (((res.dist).[i]).[j]))) + "\t") + (unbox<string> (pathStr p)))
        j <- j + 1
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
