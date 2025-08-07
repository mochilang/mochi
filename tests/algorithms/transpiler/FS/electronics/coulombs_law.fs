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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let COULOMBS_CONSTANT: float = 8988000000.0
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec coulombs_law (force: float) (charge1: float) (charge2: float) (distance: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable force = force
    let mutable charge1 = charge1
    let mutable charge2 = charge2
    let mutable distance = distance
    try
        let charge_product: float = abs (charge1 * charge2)
        let mutable zero_count: int = 0
        if force = 0.0 then
            zero_count <- zero_count + 1
        if charge1 = 0.0 then
            zero_count <- zero_count + 1
        if charge2 = 0.0 then
            zero_count <- zero_count + 1
        if distance = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("One and only one argument must be 0")
        if distance < 0.0 then
            failwith ("Distance cannot be negative")
        if force = 0.0 then
            let f: float = (COULOMBS_CONSTANT * charge_product) / (distance * distance)
            __ret <- _dictCreate [("force", f)]
            raise Return
        if charge1 = 0.0 then
            let c1: float = ((abs (force)) * (distance * distance)) / (COULOMBS_CONSTANT * charge2)
            __ret <- _dictCreate [("charge1", c1)]
            raise Return
        if charge2 = 0.0 then
            let c2: float = ((abs (force)) * (distance * distance)) / (COULOMBS_CONSTANT * charge1)
            __ret <- _dictCreate [("charge2", c2)]
            raise Return
        let d: float = sqrtApprox ((COULOMBS_CONSTANT * charge_product) / (abs (force)))
        __ret <- _dictCreate [("distance", d)]
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_map (m: System.Collections.Generic.IDictionary<string, float>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        for k in m.Keys do
            printfn "%s" (((("{\"" + k) + "\": ") + (_str (m.[(string (k))]))) + "}")
        __ret
    with
        | Return -> __ret
print_map (coulombs_law (0.0) (3.0) (5.0) (2000.0))
print_map (coulombs_law (10.0) (3.0) (5.0) (0.0))
print_map (coulombs_law (10.0) (0.0) (5.0) (2000.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
