// Generated 2025-07-26 04:38 +0700

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
let rec push (h: Map<string, int> array) (it: Map<string, int>) =
    let mutable __ret : Map<string, int> array = Unchecked.defaultof<Map<string, int> array>
    let mutable h = h
    let mutable it = it
    try
        h <- Array.append h [|it|]
        let mutable i: int = (int (Array.length h)) - 1
        while (i > 0) && (((h.[i - 1]).["s"]) > ((h.[i]).["s"])) do
            let tmp: Map<string, int> = h.[i - 1]
            h <- Map.add (i - 1) (h.[i]) h
            h <- Map.add i tmp h
            i <- i - 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and step (h: Map<string, int> array) (nv: int) (dir: int array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable h = h
    let mutable nv = nv
    let mutable dir = dir
    try
        while ((int (Array.length h)) = 0) || ((nv * nv) <= (int ((h.[0]).["s"]))) do
            h <- push h (Map.ofList [("s", nv * nv); ("a", nv); ("b", 0)])
            nv <- nv + 1
        let s = (h.[0]).["s"]
        let mutable v: int array array = [||]
        while ((int (Array.length h)) > 0) && (((h.[0]).["s"]) = s) do
            let it: Map<string, int> = h.[0]
            h <- Array.sub h 1 ((int (Array.length h)) - 1)
            v <- Array.append v [|[|it.["a"] |> unbox<int>; it.["b"] |> unbox<int>|]|]
            if (it.["a"] |> unbox<int>) > (it.["b"] |> unbox<int>) then
                h <- push h (Map.ofList [("s", ((it.["a"] |> unbox<int>) * (it.["a"] |> unbox<int>)) + (((int (it.["b"] |> unbox<int>)) + 1) * ((int (it.["b"] |> unbox<int>)) + 1))); ("a", it.["a"] |> unbox<int>); ("b", (int (it.["b"] |> unbox<int>)) + 1)])
        let mutable list: int array array = [||]
        for p in v do
            list <- Array.append list [|p|]
        let mutable temp: int array array = list
        for p in temp do
            if (p.[0]) <> (p.[1]) then
                list <- Array.append list [|[|p.[1]; p.[0]|]|]
        temp <- list
        for p in temp do
            if (int (p.[1])) <> 0 then
                list <- Array.append list [|[|p.[0]; -(p.[1])|]|]
        temp <- list
        for p in temp do
            if (int (p.[0])) <> 0 then
                list <- Array.append list [|[|-(p.[0]); p.[1]|]|]
        let mutable bestDot: int = -999999999
        let mutable best = dir
        for p in list do
            let cross = ((p.[0]) * (dir.[1])) - ((p.[1]) * (dir.[0]))
            if (int cross) >= 0 then
                let dot = ((p.[0]) * (dir.[0])) + ((p.[1]) * (dir.[1]))
                if (int dot) > bestDot then
                    bestDot <- dot
                    best <- p
        __ret <- Map.ofList [("d", box best); ("heap", box h); ("n", box nv)]
        raise Return
        __ret
    with
        | Return -> __ret
and positions (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable pos: int array array = [||]
        let mutable x: int = 0
        let mutable y: int = 0
        let mutable dir: int array = [|0; 1|]
        let mutable heap: Map<string, int> array = [||]
        let mutable nv: int = 1
        let mutable i: int = 0
        while i < n do
            pos <- Array.append pos [|[|x; y|]|]
            let st: Map<string, obj> = step heap nv dir
            dir <- unbox<int array> (st.["d"])
            heap <- unbox<Map<string, int> array> (st.["heap"])
            nv <- int (st.["n"])
            x <- x + (int (dir.[0]))
            y <- y + (int (dir.[1]))
            i <- i + 1
        __ret <- pos
        raise Return
        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable r: string = s
        while (String.length r) < w do
            r <- r + " "
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let pts: int array array = positions 40
        printfn "%s" "The first 40 Babylonian spiral points are:"
        let mutable line: string = ""
        let mutable i: int = 0
        while i < (int (Array.length pts)) do
            let p: int array = pts.[i]
            let s: string = pad (((("(" + (string (p.[0]))) + ", ") + (string (p.[1]))) + ")") 10
            line <- line + s
            if ((((i + 1) % 10 + 10) % 10)) = 0 then
                printfn "%s" line
                line <- ""
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
