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
type Result = {
    mutable _kind: string
    mutable _value: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let GRAVITATIONAL_CONSTANT: float = 0.000000000066743
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
and gravitational_law (force: float) (mass_1: float) (mass_2: float) (distance: float) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable force = force
    let mutable mass_1 = mass_1
    let mutable mass_2 = mass_2
    let mutable distance = distance
    try
        let mutable zero_count: int = 0
        if force = 0.0 then
            zero_count <- zero_count + 1
        if mass_1 = 0.0 then
            zero_count <- zero_count + 1
        if mass_2 = 0.0 then
            zero_count <- zero_count + 1
        if distance = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("One and only one argument must be 0")
        if force < 0.0 then
            failwith ("Gravitational force can not be negative")
        if distance < 0.0 then
            failwith ("Distance can not be negative")
        if mass_1 < 0.0 then
            failwith ("Mass can not be negative")
        if mass_2 < 0.0 then
            failwith ("Mass can not be negative")
        let product_of_mass: float = mass_1 * mass_2
        if force = 0.0 then
            let f: float = (GRAVITATIONAL_CONSTANT * product_of_mass) / (distance * distance)
            __ret <- { _kind = "force"; _value = f }
            raise Return
        if mass_1 = 0.0 then
            let m1: float = (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_2)
            __ret <- { _kind = "mass_1"; _value = m1 }
            raise Return
        if mass_2 = 0.0 then
            let m2: float = (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_1)
            __ret <- { _kind = "mass_2"; _value = m2 }
            raise Return
        let d: float = sqrtApprox ((GRAVITATIONAL_CONSTANT * product_of_mass) / force)
        __ret <- { _kind = "distance"; _value = d }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Result = gravitational_law (0.0) (5.0) (10.0) (20.0)
let r2: Result = gravitational_law (7367.382) (0.0) (74.0) (3048.0)
let r3: Result = gravitational_law (100.0) (5.0) (0.0) (3.0)
let r4: Result = gravitational_law (100.0) (5.0) (10.0) (0.0)
printfn "%s" (((r1._kind) + " ") + (_str (r1._value)))
printfn "%s" (((r2._kind) + " ") + (_str (r2._value)))
printfn "%s" (((r3._kind) + " ") + (_str (r3._value)))
printfn "%s" (((r4._kind) + " ") + (_str (r4._value)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
