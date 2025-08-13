// Generated 2025-08-13 16:13 +0700

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
and real_power (apparent_power: float) (power_factor: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable apparent_power = apparent_power
    let mutable power_factor = power_factor
    try
        if (power_factor < (0.0 - 1.0)) || (power_factor > 1.0) then
            ignore (failwith ("power_factor must be a valid float value between -1 and 1."))
        __ret <- apparent_power * power_factor
        raise Return
        __ret
    with
        | Return -> __ret
and reactive_power (apparent_power: float) (power_factor: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable apparent_power = apparent_power
    let mutable power_factor = power_factor
    try
        if (power_factor < (0.0 - 1.0)) || (power_factor > 1.0) then
            ignore (failwith ("power_factor must be a valid float value between -1 and 1."))
        __ret <- apparent_power * (sqrt (1.0 - (power_factor * power_factor)))
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (real_power (100.0) (0.9))))
ignore (printfn "%s" (_str (real_power (0.0) (0.8))))
ignore (printfn "%s" (_str (real_power (100.0) (-0.9))))
ignore (printfn "%s" (_str (reactive_power (100.0) (0.9))))
ignore (printfn "%s" (_str (reactive_power (0.0) (0.8))))
ignore (printfn "%s" (_str (reactive_power (100.0) (-0.9))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
