// Generated 2025-08-08 18:58 +0700

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
open System.Collections.Generic

let rec parse_decimal (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c < "0") || (c > "9") then
                failwith ("invalid literal")
            value <- int (((int64 value) * (int64 10)) + (int64 (int c)))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and zeller_day (date_input: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable date_input = date_input
    try
        let days: System.Collections.Generic.IDictionary<int, string> = unbox<System.Collections.Generic.IDictionary<int, string>> (_dictCreate [(0, "Sunday"); (1, "Monday"); (2, "Tuesday"); (3, "Wednesday"); (4, "Thursday"); (5, "Friday"); (6, "Saturday")])
        if (String.length (date_input)) <> 10 then
            failwith ("Must be 10 characters long")
        let m: int = parse_decimal (_substring date_input (0) (2))
        if (m <= 0) || (m >= 13) then
            failwith ("Month must be between 1 - 12")
        let sep1: string = string (date_input.[2])
        if (sep1 <> "-") && (sep1 <> "/") then
            failwith ("Date separator must be '-' or '/'")
        let d: int = parse_decimal (_substring date_input (3) (5))
        if (d <= 0) || (d >= 32) then
            failwith ("Date must be between 1 - 31")
        let sep2: string = string (date_input.[5])
        if (sep2 <> "-") && (sep2 <> "/") then
            failwith ("Date separator must be '-' or '/'")
        let y: int = parse_decimal (_substring date_input (6) (10))
        if (y <= 45) || (y >= 8500) then
            failwith ("Year out of range. There has to be some sort of limit...right?")
        let mutable year: int = y
        let mutable month: int = m
        if month <= 2 then
            year <- year - 1
            month <- month + 12
        let c: int = _floordiv year 100
        let k: int = ((year % 100 + 100) % 100)
        let t: int = int ((2.6 * (float month)) - 5.39)
        let u: int = _floordiv c 4
        let v: int = _floordiv k 4
        let x: int = d + k
        let z: int = ((t + u) + v) + x
        let w: int64 = (int64 z) - ((int64 2) * (int64 c))
        let mutable f: int64 = ((w % (int64 7) + (int64 7)) % (int64 7))
        if f < (int64 0) then
            f <- f + (int64 7)
        __ret <- _dictGet days ((int (f)))
        raise Return
        __ret
    with
        | Return -> __ret
and zeller (date_input: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable date_input = date_input
    try
        let day: string = zeller_day (date_input)
        __ret <- ((("Your date " + date_input) + ", is a ") + day) + "!"
        raise Return
        __ret
    with
        | Return -> __ret
and test_zeller () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let inputs: string array = unbox<string array> [|"01-31-2010"; "02-01-2010"; "11-26-2024"; "07-04-1776"|]
        let expected: string array = unbox<string array> [|"Sunday"; "Monday"; "Tuesday"; "Thursday"|]
        let mutable i: int = 0
        while i < (Seq.length (inputs)) do
            let res: string = zeller_day (_idx inputs (int i))
            if res <> (_idx expected (int i)) then
                failwith ("zeller test failed")
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_zeller()
        printfn "%s" (zeller ("01-31-2010"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
