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
let UNIVERSAL_GAS_CONSTANT: float = 8.314462
let rec pressure_of_gas_system (moles: float) (kelvin: float) (volume: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable moles = moles
    let mutable kelvin = kelvin
    let mutable volume = volume
    try
        if ((moles < (float 0)) || (kelvin < (float 0))) || (volume < (float 0)) then
            throw
            "Invalid inputs. Enter positive value."
        __ret <- ((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / volume
        raise Return
        __ret
    with
        | Return -> __ret
and volume_of_gas_system (moles: float) (kelvin: float) (pressure: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable moles = moles
    let mutable kelvin = kelvin
    let mutable pressure = pressure
    try
        if ((moles < (float 0)) || (kelvin < (float 0))) || (pressure < (float 0)) then
            throw
            "Invalid inputs. Enter positive value."
        __ret <- ((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / pressure
        raise Return
        __ret
    with
        | Return -> __ret
and temperature_of_gas_system (moles: float) (volume: float) (pressure: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable moles = moles
    let mutable volume = volume
    let mutable pressure = pressure
    try
        if ((moles < (float 0)) || (volume < (float 0))) || (pressure < (float 0)) then
            throw
            "Invalid inputs. Enter positive value."
        __ret <- (pressure * volume) / (moles * UNIVERSAL_GAS_CONSTANT)
        raise Return
        __ret
    with
        | Return -> __ret
and moles_of_gas_system (kelvin: float) (volume: float) (pressure: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable kelvin = kelvin
    let mutable volume = volume
    let mutable pressure = pressure
    try
        if ((kelvin < (float 0)) || (volume < (float 0))) || (pressure < (float 0)) then
            throw
            "Invalid inputs. Enter positive value."
        __ret <- (pressure * volume) / (kelvin * UNIVERSAL_GAS_CONSTANT)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (pressure_of_gas_system (2.0) (100.0) (5.0))
printfn "%g" (volume_of_gas_system (0.5) (273.0) (0.004))
printfn "%g" (temperature_of_gas_system (2.0) (100.0) (5.0))
printfn "%g" (moles_of_gas_system (100.0) (5.0) (10.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
