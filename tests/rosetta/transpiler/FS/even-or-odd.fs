// Generated 2025-08-04 00:30 +0700

exception Return
let mutable __ret = ()

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

let rec parseBigInt (str: string) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length (str)) > 0) && ((_substring str 0 1) = "-") then
            neg <- true
            i <- 1
        let mutable n: bigint = bigint 0
        while i < (String.length (str)) do
            let ch: string = _substring str i (i + 1)
            let d: int = int ch
            n <- (n * (bigint 10)) + (bigint d)
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string (n)
        while (String.length (s)) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and showInt (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        let mutable line: string = ("Testing integer " + (pad (n) (3))) + ":  "
        if (((n % 2 + 2) % 2)) = 0 then
            line <- line + "even "
        else
            line <- line + " odd "
        if (((n % 2 + 2) % 2)) = 0 then
            line <- line + "even"
        else
            line <- line + " odd"
        printfn "%s" (line)
        __ret
    with
        | Return -> __ret
and showBig (s: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        let b: bigint = parseBigInt (s)
        let mutable line: string = ("Testing big integer " + (string (b))) + ":  "
        if (((b % (bigint 2) + (bigint 2)) % (bigint 2))) = (bigint 0) then
            line <- line + "even"
        else
            line <- line + "odd"
        printfn "%s" (line)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        showInt (-2)
        showInt (-1)
        showInt (0)
        showInt (1)
        showInt (2)
        showBig ("-222222222222222222222222222222222222")
        showBig ("-1")
        showBig ("0")
        showBig ("1")
        showBig ("222222222222222222222222222222222222")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
