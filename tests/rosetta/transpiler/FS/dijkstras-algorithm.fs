// Generated 2025-07-28 10:03 +0700

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
let INF: int = 1000000000
let mutable graph: Map<string, Map<string, int>> = Map.ofList []
let rec addEdge (u: string) (v: string) (w: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable u = u
    let mutable v = v
    let mutable w = w
    try
        if not (Map.containsKey u graph) then
            graph <- Map.add u (Map.ofList []) graph
        graph <- Map.add u (box (Map.add v (box w) (graph.[u] |> unbox<int>>))) graph
        if not (Map.containsKey v graph) then
            graph <- Map.add v (Map.ofList []) graph
        __ret
    with
        | Return -> __ret
and removeAt (xs: string array) (idx: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable out: string array = [||]
        let mutable i: int = 0
        for x in xs do
            if i <> idx then
                out <- Array.append out [|x|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and dijkstra (source: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable source = source
    try
        let mutable dist: Map<string, int> = Map.ofList []
        let mutable prev: Map<string, string> = Map.ofList []
        for KeyValue(v, _) in graph do
            dist <- Map.add v INF dist
            prev <- Map.add v "" prev
        dist <- Map.add source 0 dist
        let mutable q: string array = [||]
        for KeyValue(v, _) in graph do
            q <- Array.append q [|v|]
        while (Seq.length q) > 0 do
            let mutable bestIdx: int = 0
            let mutable u: string = q.[0]
            let mutable i: int = 1
            while i < (Seq.length q) do
                let v: string = q.[i]
                if (dist.[v] |> unbox<int>) < (dist.[u] |> unbox<int>) then
                    u <- v
                    bestIdx <- i
                i <- i + 1
            q <- removeAt q bestIdx
            for v in graph.[u] |> unbox<int>> do
                let alt = (dist.[u] |> unbox<int>) + ((graph.[u] |> unbox<int>>).[v])
                if alt < (dist.[v] |> unbox<int>) then
                    dist <- Map.add v alt dist
                    prev <- Map.add v u prev
        __ret <- unbox<Map<string, obj>> (Map.ofList [("dist", box dist); ("prev", box prev)])
        raise Return
        __ret
    with
        | Return -> __ret
and path (prev: Map<string, string>) (v: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable prev = prev
    let mutable v = v
    try
        let mutable s: string = v
        let mutable cur: string = v
        while (unbox<string> (prev.[cur] |> unbox<string>)) <> "" do
            cur <- unbox<string> (prev.[cur] |> unbox<string>)
            s <- cur + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        addEdge "a" "b" 7
        addEdge "a" "c" 9
        addEdge "a" "f" 14
        addEdge "b" "c" 10
        addEdge "b" "d" 15
        addEdge "c" "d" 11
        addEdge "c" "f" 2
        addEdge "d" "e" 6
        addEdge "e" "f" 9
        let res: Map<string, obj> = dijkstra "a"
        let dist: Map<string, int> = unbox<Map<string, int>> (res.["dist"])
        let prev: Map<string, string> = unbox<Map<string, string>> (res.["prev"])
        printfn "%s" ((("Distance to e: " + (string (dist.["e"] |> unbox<int>))) + ", Path: ") + (unbox<string> (path prev "e")))
        printfn "%s" ((("Distance to f: " + (string (dist.["f"] |> unbox<int>))) + ", Path: ") + (unbox<string> (path prev "f")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
