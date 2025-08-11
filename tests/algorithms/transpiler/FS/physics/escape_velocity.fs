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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable k: int = 0
        if n >= 0 then
            while k < n do
                p <- p * 10.0
                k <- k + 1
        else
            let m: int = -n
            while k < m do
                p <- p / 10.0
                k <- k + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrt_newton (n: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        if n = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable x: float = n
        let mutable j: int = 0
        while j < 20 do
            x <- (x + (n / x)) / 2.0
            j <- j + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec round3 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = (x * 1000.0) + 0.5
        let mutable yi: int = int y
        if (float yi) > y then
            yi <- yi - 1
        __ret <- (float yi) / 1000.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec escape_velocity (mass: float) (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable radius = radius
    try
        if radius = 0.0 then
            failwith ("Radius cannot be zero.")
        let G: float = 6.6743 * (pow10 (-11))
        let velocity: float = sqrt_newton (((2.0 * G) * mass) / radius)
        __ret <- round3 (velocity)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (escape_velocity (5.972 * (pow10 (24))) (6.371 * (pow10 (6))))
printfn "%g" (escape_velocity (7.348 * (pow10 (22))) (1.737 * (pow10 (6))))
printfn "%g" (escape_velocity (1.898 * (pow10 (27))) (6.9911 * (pow10 (7))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
