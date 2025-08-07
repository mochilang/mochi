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
let rec decimal_to_binary_iterative (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        if num = 0 then
            __ret <- "0b0"
            raise Return
        let mutable negative: bool = false
        let mutable n: int = num
        if n < 0 then
            negative <- true
            n <- -n
        let mutable result: string = ""
        while n > 0 do
            result <- (_str (((n % 2 + 2) % 2))) + result
            n <- n / 2
        if negative then
            __ret <- "-0b" + result
            raise Return
        __ret <- "0b" + result
        raise Return
        __ret
    with
        | Return -> __ret
let rec decimal_to_binary_recursive_helper (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        if n = 1 then
            __ret <- "1"
            raise Return
        let div: int = n / 2
        let ``mod``: int = ((n % 2 + 2) % 2)
        __ret <- (decimal_to_binary_recursive_helper (div)) + (_str (``mod``))
        raise Return
        __ret
    with
        | Return -> __ret
let rec decimal_to_binary_recursive (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        if num = 0 then
            __ret <- "0b0"
            raise Return
        if num < 0 then
            __ret <- "-0b" + (decimal_to_binary_recursive_helper (-num))
            raise Return
        __ret <- "0b" + (decimal_to_binary_recursive_helper (num))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (decimal_to_binary_iterative (0))
printfn "%s" (decimal_to_binary_iterative (2))
printfn "%s" (decimal_to_binary_iterative (7))
printfn "%s" (decimal_to_binary_iterative (35))
printfn "%s" (decimal_to_binary_iterative (-2))
printfn "%s" (decimal_to_binary_recursive (0))
printfn "%s" (decimal_to_binary_recursive (40))
printfn "%s" (decimal_to_binary_recursive (-40))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
