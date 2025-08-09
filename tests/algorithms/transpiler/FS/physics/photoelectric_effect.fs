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
let rec pow10 (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let PLANCK_CONSTANT_JS: float = 6.6261 / (pow10 (34))
let PLANCK_CONSTANT_EVS: float = 4.1357 / (pow10 (15))
let rec maximum_kinetic_energy (frequency: float) (work_function: float) (in_ev: bool) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable frequency = frequency
    let mutable work_function = work_function
    let mutable in_ev = in_ev
    try
        if frequency < 0.0 then
            failwith ("Frequency can't be negative.")
        let energy: float = if in_ev then ((PLANCK_CONSTANT_EVS * frequency) - work_function) else ((PLANCK_CONSTANT_JS * frequency) - work_function)
        if energy > 0.0 then
            __ret <- energy
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (maximum_kinetic_energy (1000000.0) (2.0) (false)))
printfn "%s" (_str (maximum_kinetic_energy (1000000.0) (2.0) (true)))
printfn "%s" (_str (maximum_kinetic_energy (10000000000000000.0) (2.0) (true)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
