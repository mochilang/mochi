// Generated 2025-08-09 23:14 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec centripetal (mass: float) (velocity: float) (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable velocity = velocity
    let mutable radius = radius
    try
        if mass < 0.0 then
            failwith ("The mass of the body cannot be negative")
        if radius <= 0.0 then
            failwith ("The radius is always a positive non zero integer")
        __ret <- ((mass * velocity) * velocity) / radius
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec show (mass: float) (velocity: float) (radius: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mass = mass
    let mutable velocity = velocity
    let mutable radius = radius
    try
        let f: float = centripetal (mass) (velocity) (radius)
        printfn "%s" (_str (round (f) (2)))
        __ret
    with
        | Return -> __ret
show (15.5) (-30.0) (10.0)
show (10.0) (15.0) (5.0)
show (20.0) (-50.0) (15.0)
show (12.25) (40.0) (25.0)
show (50.0) (100.0) (50.0)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
