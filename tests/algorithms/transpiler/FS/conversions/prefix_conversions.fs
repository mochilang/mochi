// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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

let SI_UNITS: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("yotta", 24); ("zetta", 21); ("exa", 18); ("peta", 15); ("tera", 12); ("giga", 9); ("mega", 6); ("kilo", 3); ("hecto", 2); ("deca", 1); ("deci", -1); ("centi", -2); ("milli", -3); ("micro", -6); ("nano", -9); ("pico", -12); ("femto", -15); ("atto", -18); ("zepto", -21); ("yocto", -24)]
let BINARY_UNITS: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("yotta", 8); ("zetta", 7); ("exa", 6); ("peta", 5); ("tera", 4); ("giga", 3); ("mega", 2); ("kilo", 1)]
let rec pow (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        if exp = 0 then
            __ret <- 1.0
            raise Return
        let mutable e: int = exp
        if e < 0 then
            e <- -e
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < e do
            result <- result * ``base``
            i <- i + 1
        if exp < 0 then
            __ret <- 1.0 / result
            raise Return
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_si_prefix (known_amount: float) (known_prefix: string) (unknown_prefix: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable known_amount = known_amount
    let mutable known_prefix = known_prefix
    let mutable unknown_prefix = unknown_prefix
    try
        let kp: string = known_prefix.ToLower()
        let up: string = unknown_prefix.ToLower()
        if not (SI_UNITS.ContainsKey(kp)) then
            failwith ("unknown prefix: " + known_prefix)
        if not (SI_UNITS.ContainsKey(up)) then
            failwith ("unknown prefix: " + unknown_prefix)
        let diff: int = (SI_UNITS.[(string (kp))]) - (SI_UNITS.[(string (up))])
        __ret <- known_amount * (pow (10.0) (diff))
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_binary_prefix (known_amount: float) (known_prefix: string) (unknown_prefix: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable known_amount = known_amount
    let mutable known_prefix = known_prefix
    let mutable unknown_prefix = unknown_prefix
    try
        let kp: string = known_prefix.ToLower()
        let up: string = unknown_prefix.ToLower()
        if not (BINARY_UNITS.ContainsKey(kp)) then
            failwith ("unknown prefix: " + known_prefix)
        if not (BINARY_UNITS.ContainsKey(up)) then
            failwith ("unknown prefix: " + unknown_prefix)
        let diff: int = ((BINARY_UNITS.[(string (kp))]) - (BINARY_UNITS.[(string (up))])) * 10
        __ret <- known_amount * (pow (2.0) (diff))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (convert_si_prefix (1.0) ("giga") ("mega")))
printfn "%s" (_str (convert_si_prefix (1.0) ("mega") ("giga")))
printfn "%s" (_str (convert_si_prefix (1.0) ("kilo") ("kilo")))
printfn "%s" (_str (convert_binary_prefix (1.0) ("giga") ("mega")))
printfn "%s" (_str (convert_binary_prefix (1.0) ("mega") ("giga")))
printfn "%s" (_str (convert_binary_prefix (1.0) ("kilo") ("kilo")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
