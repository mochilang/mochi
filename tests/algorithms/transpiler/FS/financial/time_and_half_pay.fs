// Generated 2025-08-07 16:27 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec pay (hours_worked: float) (pay_rate: float) (hours: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable hours_worked = hours_worked
    let mutable pay_rate = pay_rate
    let mutable hours = hours
    try
        let normal_pay: float = hours_worked * pay_rate
        let mutable over_time: float = hours_worked - hours
        if over_time < 0.0 then
            over_time <- 0.0
        let over_time_pay: float = (over_time * pay_rate) / 2.0
        __ret <- normal_pay + over_time_pay
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (pay (41.0) (1.0) (40.0)))
        printfn "%s" (_str (pay (65.0) (19.0) (40.0)))
        printfn "%s" (_str (pay (10.0) (1.0) (40.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
