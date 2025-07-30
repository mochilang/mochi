// Generated 2025-07-30 21:05 +0700

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
open System

let rec leapYear (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable y = y
    try
        __ret <- (((((y % 4 + 4) % 4)) = 0) && ((((y % 100 + 100) % 100)) <> 0)) || ((((y % 400 + 400) % 400)) = 0)
        raise Return
        __ret
    with
        | Return -> __ret
and monthDays (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    try
        let mutable days: int array = [|0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
        if (m = 2) && (unbox<bool> (leapYear y)) then
            __ret <- 29
            raise Return
        __ret <- days.[m]
        raise Return
        __ret
    with
        | Return -> __ret
and zeller (y: int) (m: int) (d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        let mutable mm: int = m
        let mutable yy: int = y
        if mm < 3 then
            mm <- mm + 12
            yy <- yy - 1
        let K: int = ((yy % 100 + 100) % 100)
        let J: int = yy / 100
        let h: int = (((((((d + ((13 * (mm + 1)) / 5)) + K) + (K / 4)) + (J / 4)) + (5 * J)) % 7 + 7) % 7)
        __ret <- (((h + 6) % 7 + 7) % 7)
        raise Return
        __ret
    with
        | Return -> __ret
and lastSunday (y: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    try
        let mutable day: int = monthDays y m
        while (day > 0) && ((int (zeller y m day)) <> 0) do
            day <- day - 1
        __ret <- day
        raise Return
        __ret
    with
        | Return -> __ret
and monthName (m: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable names: string array = [|""; "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]
        __ret <- names.[m]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let year: int = int (System.Console.ReadLine())
        printfn "%s" ("Last Sundays of each month of " + (string year))
        printfn "%s" "=================================="
        let mutable m: int = 1
        while m <= 12 do
            let day: int = lastSunday year m
            printfn "%s" (((unbox<string> (monthName m)) + ": ") + (string day))
            m <- m + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
