// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Result = {
    kind: string
    value: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let ELECTRON_CHARGE: float = 0.00000000000000000016021
let rec electric_conductivity (conductivity: float) (electron_conc: float) (mobility: float) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable conductivity = conductivity
    let mutable electron_conc = electron_conc
    let mutable mobility = mobility
    try
        let mutable zero_count: int = 0
        if conductivity = 0.0 then
            zero_count <- zero_count + 1
        if electron_conc = 0.0 then
            zero_count <- zero_count + 1
        if mobility = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("You cannot supply more or less than 2 values")
        if conductivity < 0.0 then
            failwith ("Conductivity cannot be negative")
        if electron_conc < 0.0 then
            failwith ("Electron concentration cannot be negative")
        if mobility < 0.0 then
            failwith ("mobility cannot be negative")
        if conductivity = 0.0 then
            __ret <- { kind = "conductivity"; value = (mobility * electron_conc) * ELECTRON_CHARGE }
            raise Return
        if electron_conc = 0.0 then
            __ret <- { kind = "electron_conc"; value = conductivity / (mobility * ELECTRON_CHARGE) }
            raise Return
        __ret <- { kind = "mobility"; value = conductivity / (electron_conc * ELECTRON_CHARGE) }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Result = electric_conductivity (25.0) (100.0) (0.0)
let r2: Result = electric_conductivity (0.0) (1600.0) (200.0)
let r3: Result = electric_conductivity (1000.0) (0.0) (1200.0)
printfn "%s" (((r1.kind) + " ") + (_str (r1.value)))
printfn "%s" (((r2.kind) + " ") + (_str (r2.value)))
printfn "%s" (((r3.kind) + " ") + (_str (r3.value)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
