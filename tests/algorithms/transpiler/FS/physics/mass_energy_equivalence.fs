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
let C: float = 299792458.0
let rec energy_from_mass (mass: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    try
        if mass < 0.0 then
            failwith ("Mass can't be negative.")
        __ret <- (mass * C) * C
        raise Return
        __ret
    with
        | Return -> __ret
let rec mass_from_energy (energy: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable energy = energy
    try
        if energy < 0.0 then
            failwith ("Energy can't be negative.")
        __ret <- energy / (C * C)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (energy_from_mass (124.56)))
printfn "%s" (_str (energy_from_mass (320.0)))
printfn "%s" (_str (energy_from_mass (0.0)))
printfn "%s" (_str (mass_from_energy (124.56)))
printfn "%s" (_str (mass_from_energy (320.0)))
printfn "%s" (_str (mass_from_energy (0.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
