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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let G: float = 9.80665
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and terminal_velocity (mass: float) (density: float) (area: float) (drag_coefficient: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable density = density
    let mutable area = area
    let mutable drag_coefficient = drag_coefficient
    try
        if (((mass <= 0.0) || (density <= 0.0)) || (area <= 0.0)) || (drag_coefficient <= 0.0) then
            failwith ("mass, density, area and the drag coefficient all need to be positive")
        let numerator: float = (2.0 * mass) * G
        let denominator: float = (density * area) * drag_coefficient
        let result: float = sqrt (numerator / denominator)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (terminal_velocity (1.0) (25.0) (0.6) (0.77)))
printfn "%s" (_str (terminal_velocity (2.0) (100.0) (0.45) (0.23)))
printfn "%s" (_str (terminal_velocity (5.0) (50.0) (0.2) (0.5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
