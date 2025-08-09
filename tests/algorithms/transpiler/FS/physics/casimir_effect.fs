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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let PI: float = 3.141592653589793
let REDUCED_PLANCK_CONSTANT: float = 0.0000000000000000000000000000000001054571817
let SPEED_OF_LIGHT: float = 300000000.0
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 100 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and casimir_force (force: float) (area: float) (distance: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable force = force
    let mutable area = area
    let mutable distance = distance
    try
        let mutable zero_count: int = 0
        if force = 0.0 then
            zero_count <- zero_count + 1
        if area = 0.0 then
            zero_count <- zero_count + 1
        if distance = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("One and only one argument must be 0")
        if force < 0.0 then
            failwith ("Magnitude of force can not be negative")
        if distance < 0.0 then
            failwith ("Distance can not be negative")
        if area < 0.0 then
            failwith ("Area can not be negative")
        if force = 0.0 then
            let num: float = (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * area
            let den: float = (((240.0 * distance) * distance) * distance) * distance
            let f: float = num / den
            __ret <- _dictCreate [("force", f)]
            raise Return
        if area = 0.0 then
            let num: float = ((((240.0 * force) * distance) * distance) * distance) * distance
            let den: float = ((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI
            let a: float = num / den
            __ret <- _dictCreate [("area", a)]
            raise Return
        let num: float = (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * area
        let den: float = 240.0 * force
        let inner: float = num / den
        let d: float = sqrtApprox (sqrtApprox (inner))
        __ret <- _dictCreate [("distance", d)]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (casimir_force (0.0) (4.0) (0.03)))
        printfn "%s" (_str (casimir_force (0.0000000002635) (0.0023) (0.0)))
        printfn "%s" (_str (casimir_force (0.000000000000000002737) (0.0) (0.0023746)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
