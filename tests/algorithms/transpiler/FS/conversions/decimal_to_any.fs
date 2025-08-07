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

let rec decimal_to_any (num: int) (``base``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    let mutable ``base`` = ``base``
    try
        if num < 0 then
            failwith ("parameter must be positive int")
        if ``base`` < 2 then
            failwith ("base must be >= 2")
        if ``base`` > 36 then
            failwith ("base must be <= 36")
        if num = 0 then
            __ret <- "0"
            raise Return
        let symbols: string = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable n: int = num
        let mutable result: string = ""
        while n > 0 do
            let ``mod``: int = ((n % ``base`` + ``base``) % ``base``)
            let digit: string = _substring symbols ``mod`` (``mod`` + 1)
            result <- digit + result
            n <- n / ``base``
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (decimal_to_any (0) (2))
        printfn "%s" (decimal_to_any (5) (4))
        printfn "%s" (decimal_to_any (20) (3))
        printfn "%s" (decimal_to_any (58) (16))
        printfn "%s" (decimal_to_any (243) (17))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
