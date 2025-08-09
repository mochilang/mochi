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
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
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
and cos (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
and radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
and abs_val (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and malus_law (initial_intensity: float) (angle: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable initial_intensity = initial_intensity
    let mutable angle = angle
    try
        if initial_intensity < 0.0 then
            failwith ("The value of intensity cannot be negative")
        if (angle < 0.0) || (angle > 360.0) then
            failwith ("In Malus Law, the angle is in the range 0-360 degrees")
        let theta: float = radians (angle)
        let c: float = cos (theta)
        __ret <- initial_intensity * (c * c)
        raise Return
        __ret
    with
        | Return -> __ret
and test_malus_law () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (abs_val ((malus_law (10.0) (45.0)) - 5.0)) > 0.01 then
            failwith ("malus_law test1 failed")
        if (abs_val ((malus_law (100.0) (60.0)) - 25.0)) > 0.01 then
            failwith ("malus_law test2 failed")
        if (abs_val ((malus_law (50.0) (150.0)) - 37.5)) > 0.01 then
            failwith ("malus_law test3 failed")
        if (abs_val ((malus_law (75.0) (270.0)) - 0.0)) > 0.01 then
            failwith ("malus_law test4 failed")
        if (abs_val ((malus_law (100.0) (180.0)) - 100.0)) > 0.01 then
            failwith ("malus_law test5 failed")
        if (abs_val ((malus_law (100.0) (360.0)) - 100.0)) > 0.01 then
            failwith ("malus_law test6 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_malus_law()
        printfn "%s" (_str (malus_law (100.0) (60.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
