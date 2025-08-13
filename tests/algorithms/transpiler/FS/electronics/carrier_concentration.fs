// Generated 2025-08-13 07:12 +0700

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
type CarrierResult = {
    mutable _name: string
    mutable _value: float
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
and carrier_concentration (electron_conc: float) (hole_conc: float) (intrinsic_conc: float) =
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
            ignore (failwith ("You cannot supply more or less than 2 values"))
        if electron_conc < 0.0 then
            ignore (failwith ("Electron concentration cannot be negative in a semiconductor"))
        if hole_conc < 0.0 then
            ignore (failwith ("Hole concentration cannot be negative in a semiconductor"))
        if intrinsic_conc < 0.0 then
            ignore (failwith ("Intrinsic concentration cannot be negative in a semiconductor"))
        if electron_conc = 0.0 then
            __ret <- { _name = "electron_conc"; _value = (intrinsic_conc * intrinsic_conc) / hole_conc }
            raise Return
        if hole_conc = 0.0 then
            __ret <- { _name = "hole_conc"; _value = (intrinsic_conc * intrinsic_conc) / electron_conc }
            raise Return
        if intrinsic_conc = 0.0 then
            __ret <- { _name = "intrinsic_conc"; _value = sqrtApprox (electron_conc * hole_conc) }
            raise Return
        __ret <- { _name = ""; _value = -1.0 }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: CarrierResult = carrier_concentration (25.0) (100.0) (0.0)
ignore (printfn "%s" (((r1._name) + ", ") + (_str (r1._value))))
let r2: CarrierResult = carrier_concentration (0.0) (1600.0) (200.0)
ignore (printfn "%s" (((r2._name) + ", ") + (_str (r2._value))))
let r3: CarrierResult = carrier_concentration (1000.0) (0.0) (1200.0)
ignore (printfn "%s" (((r3._name) + ", ") + (_str (r3._value))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
