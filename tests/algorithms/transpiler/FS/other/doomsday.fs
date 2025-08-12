// Generated 2025-08-12 16:24 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let DOOMSDAY_LEAP: int array = unbox<int array> [|4; 1; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5|]
let DOOMSDAY_NOT_LEAP: int array = unbox<int array> [|3; 7; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5|]
let WEEK_DAY_NAMES: System.Collections.Generic.IDictionary<int, string> = unbox<System.Collections.Generic.IDictionary<int, string>> (_dictCreate [(0, "Sunday"); (1, "Monday"); (2, "Tuesday"); (3, "Wednesday"); (4, "Thursday"); (5, "Friday"); (6, "Saturday")])
let rec get_week_day (year: int) (month: int) (day: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable year = year
    let mutable month = month
    let mutable day = day
    try
        if year < 100 then
            ignore (failwith ("year should be in YYYY format"))
        if (month < 1) || (month > 12) then
            ignore (failwith ("month should be between 1 to 12"))
        if (day < 1) || (day > 31) then
            ignore (failwith ("day should be between 1 to 31"))
        let century: int = _floordiv year 100
        let century_anchor: int = ((((5 * (((century % 4 + 4) % 4))) + 2) % 7 + 7) % 7)
        let centurian: int = ((year % 100 + 100) % 100)
        let centurian_m: int = ((centurian % 12 + 12) % 12)
        let dooms_day: int = ((((((_floordiv centurian 12) + centurian_m) + (_floordiv centurian_m 4)) + century_anchor) % 7 + 7) % 7)
        let day_anchor: int = if ((((year % 4 + 4) % 4)) <> 0) || ((centurian = 0) && ((((year % 400 + 400) % 400)) <> 0)) then (_idx DOOMSDAY_NOT_LEAP (int (month - 1))) else (_idx DOOMSDAY_LEAP (int (month - 1)))
        let mutable week_day: int = ((((dooms_day + day) - day_anchor) % 7 + 7) % 7)
        if week_day < 0 then
            week_day <- week_day + 7
        __ret <- _dictGet WEEK_DAY_NAMES (week_day)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (get_week_day (2020) (10) (24)))
ignore (printfn "%s" (get_week_day (2017) (10) (24)))
ignore (printfn "%s" (get_week_day (2019) (5) (3)))
ignore (printfn "%s" (get_week_day (1970) (9) (16)))
ignore (printfn "%s" (get_week_day (1870) (8) (13)))
ignore (printfn "%s" (get_week_day (2040) (3) (14)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
