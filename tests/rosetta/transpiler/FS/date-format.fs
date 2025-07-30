// Generated 2025-07-30 21:41 +0700

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

let rec pad2 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- if n < 10 then ("0" + (string n)) else (string n)
        raise Return
        __ret
    with
        | Return -> __ret
and weekdayName (z: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable z = z
    try
        let names: string array = [|"Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"|]
        __ret <- names.[(((z + 4) % 7 + 7) % 7)]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ts: int = int ((int (_now())) / 1000000000)
        let mutable days: int = int (ts / 86400)
        let mutable z: int = days + 719468
        let mutable era: int = int (z / 146097)
        let mutable doe: int = z - (era * 146097)
        let mutable yoe: int = (((doe - (doe / 1460)) + (doe / 36524)) - (doe / 146096)) / (int 365)
        let mutable y: int = yoe + (era * 400)
        let mutable doy: int = doe - (((365 * yoe) + (yoe / 4)) - (yoe / 100))
        let mutable mp: int = ((5 * doy) + 2) / (int 153)
        let mutable d: int = int ((doy - (((153 * mp) + 2) / (int 5))) + 1)
        let mutable m: int = int (mp + 3)
        if m > 12 then
            y <- y + 1
            m <- m - 12
        let iso: string = ((((string y) + "-") + (unbox<string> (pad2 m))) + "-") + (unbox<string> (pad2 d))
        printfn "%s" iso
        let months: string array = [|"January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]
        let line: string = ((((((unbox<string> (weekdayName days)) + ", ") + (months.[m - 1])) + " ") + (string d)) + ", ") + (string y)
        printfn "%s" line
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
