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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let EQUATORIAL_RADIUS: float = 6378137.0
let rec to_radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrt_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec lamberts_ellipsoidal_distance (lat1: float) (lon1: float) (lat2: float) (lon2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable lat1 = lat1
    let mutable lon1 = lon1
    let mutable lat2 = lat2
    let mutable lon2 = lon2
    try
        let phi1: float = to_radians (lat1)
        let phi2: float = to_radians (lat2)
        let lambda1: float = to_radians (lon1)
        let lambda2: float = to_radians (lon2)
        let x: float = (lambda2 - lambda1) * (cos_approx ((phi1 + phi2) / 2.0))
        let y: float = phi2 - phi1
        __ret <- EQUATORIAL_RADIUS * (sqrt_approx ((x * x) + (y * y)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (lamberts_ellipsoidal_distance (37.774856) (-122.424227) (37.864742) (-119.537521))
printfn "%g" (lamberts_ellipsoidal_distance (37.774856) (-122.424227) (40.713019) (-74.012647))
printfn "%g" (lamberts_ellipsoidal_distance (37.774856) (-122.424227) (45.443012) (12.313071))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
