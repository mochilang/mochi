// Generated 2025-08-09 16:21 +0700

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
let G: float = 0.000000000066743
let C: float = 299792458.0
let PI: float = 3.141592653589793
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and capture_radii (target_body_radius: float) (target_body_mass: float) (projectile_velocity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable target_body_radius = target_body_radius
    let mutable target_body_mass = target_body_mass
    let mutable projectile_velocity = projectile_velocity
    try
        if target_body_mass < 0.0 then
            failwith ("Mass cannot be less than 0")
        if target_body_radius < 0.0 then
            failwith ("Radius cannot be less than 0")
        if projectile_velocity > C then
            failwith ("Cannot go beyond speed of light")
        let escape_velocity_squared: float = ((2.0 * G) * target_body_mass) / target_body_radius
        let denom: float = projectile_velocity * projectile_velocity
        let capture_radius: float = target_body_radius * (sqrt (1.0 + (escape_velocity_squared / denom)))
        __ret <- capture_radius
        raise Return
        __ret
    with
        | Return -> __ret
and capture_area (capture_radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable capture_radius = capture_radius
    try
        if capture_radius < 0.0 then
            failwith ("Cannot have a capture radius less than 0")
        let sigma: float = (PI * capture_radius) * capture_radius
        __ret <- sigma
        raise Return
        __ret
    with
        | Return -> __ret
and run_tests () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let r: float = capture_radii (6.957 * (pow10 (8))) (1.99 * (pow10 (30))) (25000.0)
        if (abs (r - (1.720959069143714 * (pow10 (10))))) > 1.0 then
            failwith ("capture_radii failed")
        let a: float = capture_area (r)
        if (abs (a - (9.304455331801812 * (pow10 (20))))) > 1.0 then
            failwith ("capture_area failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        run_tests()
        let r: float = capture_radii (6.957 * (pow10 (8))) (1.99 * (pow10 (30))) (25000.0)
        printfn "%s" (_str (r))
        printfn "%s" (_str (capture_area (r)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
