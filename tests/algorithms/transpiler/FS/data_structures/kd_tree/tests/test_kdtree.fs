// Generated 2025-08-07 14:57 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

let INF: float = 1000000000.0
let mutable _seed: int = 1
let rec rand_float () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (float _seed) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and hypercube_points (num_points: int) (cube_size: float) (num_dimensions: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable num_points = num_points
    let mutable cube_size = cube_size
    let mutable num_dimensions = num_dimensions
    try
        let mutable pts: float array array = [||]
        let mutable i: int = 0
        while i < num_points do
            let mutable p: float array = [||]
            let mutable j: int = 0
            while j < num_dimensions do
                let v: float = cube_size * (rand_float())
                p <- Array.append p [|v|]
                j <- j + 1
            pts <- Array.append pts [|p|]
            i <- i + 1
        __ret <- pts
        raise Return
        __ret
    with
        | Return -> __ret
and build_kdtree (points: float array array) (depth: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable points = points
    let mutable depth = depth
    try
        __ret <- points
        raise Return
        __ret
    with
        | Return -> __ret
and distance_sq (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let d: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (d * d)
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and nearest_neighbour_search (points: float array array) (query: float array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable points = points
    let mutable query = query
    try
        if (Seq.length (points)) = 0 then
            __ret <- _dictCreate [("index", -1.0); ("dist", INF); ("visited", 0.0)]
            raise Return
        let mutable nearest_idx: int = 0
        let mutable nearest_dist: float = INF
        let mutable visited: int = 0
        let mutable i: int = 0
        while i < (Seq.length (points)) do
            let d: float = distance_sq (query) (_idx points (i))
            visited <- visited + 1
            if d < nearest_dist then
                nearest_dist <- d
                nearest_idx <- i
            i <- i + 1
        __ret <- _dictCreate [("index", float nearest_idx); ("dist", nearest_dist); ("visited", float visited)]
        raise Return
        __ret
    with
        | Return -> __ret
and test_build_cases () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let empty_pts: float array array = [||]
        let tree0: float array array = build_kdtree (empty_pts) (0)
        if (Seq.length (tree0)) = 0 then
            printfn "%s" ("case1 true")
        else
            printfn "%s" ("case1 false")
        let pts1: float array array = hypercube_points (10) (10.0) (2)
        let tree1: float array array = build_kdtree (pts1) (2)
        if ((Seq.length (tree1)) > 0) && ((Seq.length (_idx tree1 (0))) = 2) then
            printfn "%s" ("case2 true")
        else
            printfn "%s" ("case2 false")
        let pts2: float array array = hypercube_points (10) (10.0) (3)
        let tree2: float array array = build_kdtree (pts2) (-2)
        if ((Seq.length (tree2)) > 0) && ((Seq.length (_idx tree2 (0))) = 3) then
            printfn "%s" ("case3 true")
        else
            printfn "%s" ("case3 false")
        __ret
    with
        | Return -> __ret
and test_search () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable pts: float array array = hypercube_points (10) (10.0) (2)
        let tree: float array array = build_kdtree (pts) (0)
        let qp: float array = _idx (hypercube_points (1) (10.0) (2)) (0)
        let res: System.Collections.Generic.IDictionary<string, float> = nearest_neighbour_search (tree) (qp)
        if (((res.[(string ("index"))]) <> (-1.0)) && ((res.[(string ("dist"))]) >= 0.0)) && ((res.[(string ("visited"))]) > 0.0) then
            printfn "%s" ("search true")
        else
            printfn "%s" ("search false")
        __ret
    with
        | Return -> __ret
and test_edge () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let empty_pts: float array array = [||]
        let tree: float array array = build_kdtree (empty_pts) (0)
        let query: float array = [|0.0; 0.0|]
        let res: System.Collections.Generic.IDictionary<string, float> = nearest_neighbour_search (tree) (query)
        if (((res.[(string ("index"))]) = (-1.0)) && ((res.[(string ("dist"))]) > 100000000.0)) && ((res.[(string ("visited"))]) = 0.0) then
            printfn "%s" ("edge true")
        else
            printfn "%s" ("edge false")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        _seed <- 1
        test_build_cases()
        test_search()
        test_edge()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
