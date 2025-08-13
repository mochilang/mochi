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
type Result = {
    mutable _kind: string
    mutable _value: float
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
            ignore (failwith ("You cannot supply more or less than 2 values"))
        if conductivity < 0.0 then
            ignore (failwith ("Conductivity cannot be negative"))
        if electron_conc < 0.0 then
            ignore (failwith ("Electron concentration cannot be negative"))
        if mobility < 0.0 then
            ignore (failwith ("mobility cannot be negative"))
        if conductivity = 0.0 then
            __ret <- { _kind = "conductivity"; _value = (mobility * electron_conc) * ELECTRON_CHARGE }
            raise Return
        if electron_conc = 0.0 then
            __ret <- { _kind = "electron_conc"; _value = conductivity / (mobility * ELECTRON_CHARGE) }
            raise Return
        __ret <- { _kind = "mobility"; _value = conductivity / (electron_conc * ELECTRON_CHARGE) }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Result = electric_conductivity (25.0) (100.0) (0.0)
let r2: Result = electric_conductivity (0.0) (1600.0) (200.0)
let r3: Result = electric_conductivity (1000.0) (0.0) (1200.0)
ignore (printfn "%s" (((r1._kind) + " ") + (_str (r1._value))))
ignore (printfn "%s" (((r2._kind) + " ") + (_str (r2._value))))
ignore (printfn "%s" (((r3._kind) + " ") + (_str (r3._value))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
