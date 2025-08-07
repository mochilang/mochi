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
type CarrierResult = {
    name: string
    value: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec carrier_concentration (electron_conc: float) (hole_conc: float) (intrinsic_conc: float) =
    let mutable __ret : CarrierResult = Unchecked.defaultof<CarrierResult>
    let mutable electron_conc = electron_conc
    let mutable hole_conc = hole_conc
    let mutable intrinsic_conc = intrinsic_conc
    try
        let mutable zero_count: int = 0
        if electron_conc = 0.0 then
            zero_count <- zero_count + 1
        if hole_conc = 0.0 then
            zero_count <- zero_count + 1
        if intrinsic_conc = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("You cannot supply more or less than 2 values")
        if electron_conc < 0.0 then
            failwith ("Electron concentration cannot be negative in a semiconductor")
        if hole_conc < 0.0 then
            failwith ("Hole concentration cannot be negative in a semiconductor")
        if intrinsic_conc < 0.0 then
            failwith ("Intrinsic concentration cannot be negative in a semiconductor")
        if electron_conc = 0.0 then
            __ret <- { name = "electron_conc"; value = (intrinsic_conc * intrinsic_conc) / hole_conc }
            raise Return
        if hole_conc = 0.0 then
            __ret <- { name = "hole_conc"; value = (intrinsic_conc * intrinsic_conc) / electron_conc }
            raise Return
        if intrinsic_conc = 0.0 then
            __ret <- { name = "intrinsic_conc"; value = sqrtApprox (electron_conc * hole_conc) }
            raise Return
        __ret <- { name = ""; value = -1.0 }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: CarrierResult = carrier_concentration (25.0) (100.0) (0.0)
printfn "%s" (((r1.name) + ", ") + (_str (r1.value)))
let r2: CarrierResult = carrier_concentration (0.0) (1600.0) (200.0)
printfn "%s" (((r2.name) + ", ") + (_str (r2.value)))
let r3: CarrierResult = carrier_concentration (1000.0) (0.0) (1200.0)
printfn "%s" (((r3.name) + ", ") + (_str (r3.value)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
