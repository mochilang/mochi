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
let UNIVERSAL_GAS_CONSTANT: float = 8.3144598
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
let rec rms_speed_of_molecule (temperature: float) (molar_mass: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable temperature = temperature
    let mutable molar_mass = molar_mass
    try
        if temperature < 0.0 then
            failwith ("Temperature cannot be less than 0 K")
        if molar_mass <= 0.0 then
            failwith ("Molar mass cannot be less than or equal to 0 kg/mol")
        let num: float = (3.0 * UNIVERSAL_GAS_CONSTANT) * temperature
        let ``val``: float = num / molar_mass
        let result: float = sqrt (``val``)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("rms_speed_of_molecule(100, 2) = " + (_str (rms_speed_of_molecule (100.0) (2.0))))
printfn "%s" ("rms_speed_of_molecule(273, 12) = " + (_str (rms_speed_of_molecule (273.0) (12.0))))
let vrms: float = rms_speed_of_molecule (300.0) (28.0)
printfn "%s" (("Vrms of Nitrogen gas at 300 K is " + (_str (vrms))) + " m/s")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
