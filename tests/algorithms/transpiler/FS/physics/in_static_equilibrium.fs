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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
and sin_approx (x: float) =
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
and cos_approx (x: float) =
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
and polar_force (magnitude: float) (angle: float) (radian_mode: bool) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable magnitude = magnitude
    let mutable angle = angle
    let mutable radian_mode = radian_mode
    try
        let theta: float = if radian_mode then angle else ((angle * PI) / 180.0)
        __ret <- unbox<float array> [|magnitude * (cos_approx (theta)); magnitude * (sin_approx (theta))|]
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
and in_static_equilibrium (forces: float array array) (location: float array array) (eps: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable forces = forces
    let mutable location = location
    let mutable eps = eps
    try
        let mutable sum_moments: float = 0.0
        let mutable i: int = 0
        let n: int = Seq.length (forces)
        while i < n do
            let r: float array = _idx location (int i)
            let f: float array = _idx forces (int i)
            let moment: float = ((_idx r (int 0)) * (_idx f (int 1))) - ((_idx r (int 1)) * (_idx f (int 0)))
            sum_moments <- sum_moments + moment
            i <- i + 1
        __ret <- (abs_float (sum_moments)) < eps
        raise Return
        __ret
    with
        | Return -> __ret
let forces1: float array array = [|[|1.0; 1.0|]; [|-1.0; 2.0|]|]
let location1: float array array = [|[|1.0; 0.0|]; [|10.0; 0.0|]|]
printfn "%s" (_str (in_static_equilibrium (forces1) (location1) (0.1)))
let forces2: float array array = [|polar_force (718.4) (150.0) (false); polar_force (879.54) (45.0) (false); polar_force (100.0) (-90.0) (false)|]
let location2: float array array = [|[|0.0; 0.0|]; [|0.0; 0.0|]; [|0.0; 0.0|]|]
printfn "%s" (_str (in_static_equilibrium (forces2) (location2) (0.1)))
let forces3: float array array = [|polar_force (30.0 * 9.81) (15.0) (false); polar_force (215.0) (135.0) (false); polar_force (264.0) (60.0) (false)|]
let location3: float array array = [|[|0.0; 0.0|]; [|0.0; 0.0|]; [|0.0; 0.0|]|]
printfn "%s" (_str (in_static_equilibrium (forces3) (location3) (0.1)))
let forces4: float array array = [|[|0.0; -2000.0|]; [|0.0; -1200.0|]; [|0.0; 15600.0|]; [|0.0; -12400.0|]|]
let location4: float array array = [|[|0.0; 0.0|]; [|6.0; 0.0|]; [|10.0; 0.0|]; [|12.0; 0.0|]|]
printfn "%s" (_str (in_static_equilibrium (forces4) (location4) (0.1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
