// Generated 2025-08-08 11:10 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
let rec random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (float (rand())) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec hypercube_points (num_points: int) (hypercube_size: float) (num_dimensions: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable num_points = num_points
    let mutable hypercube_size = hypercube_size
    let mutable num_dimensions = num_dimensions
    try
        let mutable points: float array array = [||]
        let mutable i: int = 0
        while i < num_points do
            let mutable point: float array = [||]
            let mutable j: int = 0
            while j < num_dimensions do
                let value: float = hypercube_size * (random())
                point <- Array.append point [|value|]
                j <- j + 1
            points <- Array.append points [|point|]
            i <- i + 1
        __ret <- points
        raise Return
        __ret
    with
        | Return -> __ret
let pts: float array array = hypercube_points (3) (1.0) (2)
printfn "%s" (_repr (pts))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
