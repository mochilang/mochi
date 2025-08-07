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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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

let UNIT_SYMBOL: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("meter", "m"); ("kilometer", "km"); ("megametre", "Mm"); ("gigametre", "Gm"); ("terametre", "Tm"); ("petametre", "Pm"); ("exametre", "Em"); ("zettametre", "Zm"); ("yottametre", "Ym")]
let METRIC_CONVERSION: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("m", 0); ("km", 3); ("Mm", 6); ("Gm", 9); ("Tm", 12); ("Pm", 15); ("Em", 18); ("Zm", 21); ("Ym", 24)]
let ABBREVIATIONS: string = "m, km, Mm, Gm, Tm, Pm, Em, Zm, Ym"
let rec sanitize (unit: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable unit = unit
    try
        let mutable res: string = unit.ToLower()
        if (String.length (res)) > 0 then
            let last: string = _substring res ((String.length (res)) - 1) (String.length (res))
            if last = "s" then
                res <- _substring res 0 ((String.length (res)) - 1)
        if UNIT_SYMBOL.ContainsKey(res) then
            __ret <- UNIT_SYMBOL.[(string (res))]
            raise Return
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow10 (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable exp = exp
    try
        if exp = 0 then
            __ret <- 1.0
            raise Return
        let mutable e: int = exp
        let mutable res: float = 1.0
        if e < 0 then
            e <- -e
        let mutable i: int = 0
        while i < e do
            res <- res * 10.0
            i <- i + 1
        if exp < 0 then
            __ret <- 1.0 / res
            raise Return
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec length_conversion (value: float) (from_type: string) (to_type: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable from_type = from_type
    let mutable to_type = to_type
    try
        let from_sanitized: string = sanitize (from_type)
        let to_sanitized: string = sanitize (to_type)
        if not (METRIC_CONVERSION.ContainsKey(from_sanitized)) then
            failwith ((("Invalid 'from_type' value: '" + from_type) + "'.\nConversion abbreviations are: ") + ABBREVIATIONS)
        if not (METRIC_CONVERSION.ContainsKey(to_sanitized)) then
            failwith ((("Invalid 'to_type' value: '" + to_type) + "'.\nConversion abbreviations are: ") + ABBREVIATIONS)
        let from_exp: int = METRIC_CONVERSION.[(string (from_sanitized))]
        let to_exp: int = METRIC_CONVERSION.[(string (to_sanitized))]
        let mutable exponent: int = 0
        if from_exp > to_exp then
            exponent <- from_exp - to_exp
        else
            exponent <- -(to_exp - from_exp)
        __ret <- value * (pow10 (exponent))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (length_conversion (1.0) ("meter") ("kilometer")))
printfn "%s" (_str (length_conversion (1.0) ("meter") ("megametre")))
printfn "%s" (_str (length_conversion (1.0) ("gigametre") ("meter")))
printfn "%s" (_str (length_conversion (1.0) ("terametre") ("zettametre")))
printfn "%s" (_str (length_conversion (1.0) ("yottametre") ("zettametre")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
