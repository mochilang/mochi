// Generated 2025-08-09 10:14 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type EasterDate = {
    mutable _month: int
    mutable _day: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gauss_easter (year: int) =
    let mutable __ret : EasterDate = Unchecked.defaultof<EasterDate>
    let mutable year = year
    try
        let metonic_cycle: int = ((year % 19 + 19) % 19)
        let julian_leap_year: int = ((year % 4 + 4) % 4)
        let non_leap_year: int = ((year % 7 + 7) % 7)
        let leap_day_inhibits: int = _floordiv year 100
        let lunar_orbit_correction: int64 = ((int64 13) + ((int64 8) * (int64 leap_day_inhibits))) / (int64 25)
        let leap_day_reinstall_number: float = (float leap_day_inhibits) / 4.0
        let secular_moon_shift: float = (((((15.0 - (float lunar_orbit_correction)) + (float leap_day_inhibits)) - leap_day_reinstall_number) % 30.0 + 30.0) % 30.0)
        let century_starting_point: float = ((((4.0 + (float leap_day_inhibits)) - leap_day_reinstall_number) % 7.0 + 7.0) % 7.0)
        let days_to_add: float = ((((19.0 * (float metonic_cycle)) + secular_moon_shift) % 30.0 + 30.0) % 30.0)
        let days_from_phm_to_sunday: float = ((((((2.0 * (float julian_leap_year)) + (4.0 * (float non_leap_year))) + (6.0 * days_to_add)) + century_starting_point) % 7.0 + 7.0) % 7.0)
        if (days_to_add = 29.0) && (days_from_phm_to_sunday = 6.0) then
            __ret <- { _month = 4; _day = 19 }
            raise Return
        if (days_to_add = 28.0) && (days_from_phm_to_sunday = 6.0) then
            __ret <- { _month = 4; _day = 18 }
            raise Return
        let offset: int = int (days_to_add + days_from_phm_to_sunday)
        let total: int = 22 + offset
        if total > 31 then
            __ret <- { _month = 4; _day = total - 31 }
            raise Return
        __ret <- { _month = 3; _day = total }
        raise Return
        __ret
    with
        | Return -> __ret
let rec format_date (year: int) (d: EasterDate) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable year = year
    let mutable d = d
    try
        let _month: string = if (d._month) < 10 then ("0" + (_str (d._month))) else (_str (d._month))
        let _day: string = if (d._day) < 10 then ("0" + (_str (d._day))) else (_str (d._day))
        __ret <- ((((_str (year)) + "-") + _month) + "-") + _day
        raise Return
        __ret
    with
        | Return -> __ret
let years: int array = unbox<int array> [|1994; 2000; 2010; 2021; 2023; 2032; 2100|]
let mutable i: int = 0
while i < (Seq.length (years)) do
    let y: int = _idx years (int i)
    let e: EasterDate = gauss_easter (y)
    printfn "%s" ((("Easter in " + (_str (y))) + " is ") + (format_date (y) (e)))
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
