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
let PI: float = 3.141592653589793
let R: float = 8.31446261815324
let rec sqrt (x: float) =
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
and avg_speed_of_molecule (temperature: float) (molar_mass: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable temperature = temperature
    let mutable molar_mass = molar_mass
    try
        if temperature < 0.0 then
            failwith ("Absolute temperature cannot be less than 0 K")
        if molar_mass <= 0.0 then
            failwith ("Molar mass should be greater than 0 kg/mol")
        let expr: float = ((8.0 * R) * temperature) / (PI * molar_mass)
        let s: float = sqrt (expr)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and mps_speed_of_molecule (temperature: float) (molar_mass: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable temperature = temperature
    let mutable molar_mass = molar_mass
    try
        if temperature < 0.0 then
            failwith ("Absolute temperature cannot be less than 0 K")
        if molar_mass <= 0.0 then
            failwith ("Molar mass should be greater than 0 kg/mol")
        let expr: float = ((2.0 * R) * temperature) / molar_mass
        let s: float = sqrt (expr)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (avg_speed_of_molecule (273.0) (0.028)))
printfn "%s" (_str (avg_speed_of_molecule (300.0) (0.032)))
printfn "%s" (_str (mps_speed_of_molecule (273.0) (0.028)))
printfn "%s" (_str (mps_speed_of_molecule (300.0) (0.032)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
