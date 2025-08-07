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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let time_chart: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("seconds", 1.0); ("minutes", 60.0); ("hours", 3600.0); ("days", 86400.0); ("weeks", 604800.0); ("months", 2629800.0); ("years", 31557600.0)]
let time_chart_inverse: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("seconds", 1.0); ("minutes", 1.0 / 60.0); ("hours", 1.0 / 3600.0); ("days", 1.0 / 86400.0); ("weeks", 1.0 / 604800.0); ("months", 1.0 / 2629800.0); ("years", 1.0 / 31557600.0)]
let units: string array = [|"seconds"; "minutes"; "hours"; "days"; "weeks"; "months"; "years"|]
let units_str: string = "seconds, minutes, hours, days, weeks, months, years"
let rec contains (arr: string array) (t: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable t = t
    try
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) = t then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_time (time_value: float) (unit_from: string) (unit_to: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable time_value = time_value
    let mutable unit_from = unit_from
    let mutable unit_to = unit_to
    try
        if time_value < 0.0 then
            failwith ("'time_value' must be a non-negative number.")
        let from: string = unit_from.ToLower()
        let ``to``: string = unit_to.ToLower()
        if (not (contains (units) (from))) || (not (contains (units) (``to``))) then
            let mutable invalid_unit: string = from
            if contains (units) (from) then
                invalid_unit <- ``to``
            failwith (((("Invalid unit " + invalid_unit) + " is not in ") + units_str) + ".")
        let seconds: float = time_value * (time_chart.[(string (from))])
        let converted: float = seconds * (time_chart_inverse.[(string (``to``))])
        let scaled: float = converted * 1000.0
        let int_part: int = int (scaled + 0.5)
        __ret <- ((float int_part) + 0.0) / 1000.0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (convert_time (3600.0) ("seconds") ("hours"))
printfn "%g" (convert_time (360.0) ("days") ("months"))
printfn "%g" (convert_time (360.0) ("months") ("years"))
printfn "%g" (convert_time (1.0) ("years") ("seconds"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
