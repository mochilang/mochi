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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
let g: float = 9.80665
let rec _mod (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((float (int (x / m))) * m)
        raise Return
        __ret
    with
        | Return -> __ret
and sin (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y3: float = y2 * y
        let y5: float = y3 * y2
        let y7: float = y5 * y2
        __ret <- ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
        raise Return
        __ret
    with
        | Return -> __ret
and deg_to_rad (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
and floor (x: float) =
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
and pow10 (n: int) =
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
and round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        let y: float = floor ((x * m) + 0.5)
        __ret <- y / m
        raise Return
        __ret
    with
        | Return -> __ret
and check_args (init_velocity: float) (angle: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable init_velocity = init_velocity
    let mutable angle = angle
    try
        if (angle > 90.0) || (angle < 1.0) then
            failwith ("Invalid angle. Range is 1-90 degrees.")
        if init_velocity < 0.0 then
            failwith ("Invalid velocity. Should be a positive number.")
        __ret
    with
        | Return -> __ret
and horizontal_distance (init_velocity: float) (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable init_velocity = init_velocity
    let mutable angle = angle
    try
        check_args (init_velocity) (angle)
        let radians: float = deg_to_rad (2.0 * angle)
        __ret <- round (((init_velocity * init_velocity) * (sin (radians))) / g) (2)
        raise Return
        __ret
    with
        | Return -> __ret
and max_height (init_velocity: float) (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable init_velocity = init_velocity
    let mutable angle = angle
    try
        check_args (init_velocity) (angle)
        let radians: float = deg_to_rad (angle)
        let s: float = sin (radians)
        __ret <- round ((((init_velocity * init_velocity) * s) * s) / (2.0 * g)) (2)
        raise Return
        __ret
    with
        | Return -> __ret
and total_time (init_velocity: float) (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable init_velocity = init_velocity
    let mutable angle = angle
    try
        check_args (init_velocity) (angle)
        let radians: float = deg_to_rad (angle)
        __ret <- round (((2.0 * init_velocity) * (sin (radians))) / g) (2)
        raise Return
        __ret
    with
        | Return -> __ret
let v0: float = 25.0
let angle: float = 20.0
printfn "%g" (horizontal_distance (v0) (angle))
printfn "%g" (max_height (v0) (angle))
printfn "%g" (total_time (v0) (angle))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
