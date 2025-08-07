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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec int_pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec decimal_to_octal (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        if num = 0 then
            __ret <- "0o0"
            raise Return
        let mutable octal: int = 0
        let mutable counter: int = 0
        let mutable value: int = num
        while value > 0 do
            let remainder: int = ((value % 8 + 8) % 8)
            octal <- octal + (remainder * (int_pow (10) (counter)))
            counter <- counter + 1
            value <- value / 8
        __ret <- "0o" + (_str (octal))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (decimal_to_octal (2))
printfn "%s" (decimal_to_octal (8))
printfn "%s" (decimal_to_octal (65))
printfn "%s" (decimal_to_octal (216))
printfn "%s" (decimal_to_octal (512))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
