// Generated 2025-08-06 20:48 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec to_binary4 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable result: string = ""
        let mutable x: int = n
        while x > 0 do
            result <- (_str (((x % 2 + 2) % 2))) + result
            x <- x / 2
        while (String.length(result)) < 4 do
            result <- "0" + result
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and binary_coded_decimal (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        let mutable n: int = number
        if n < 0 then
            n <- 0
        let digits: string = _str (n)
        let mutable out: string = "0b"
        let mutable i: int = 0
        while i < (String.length(digits)) do
            let d: string = string (digits.[i])
            let d_int: int = int d
            out <- out + (to_binary4 (d_int))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (binary_coded_decimal (-2))
printfn "%s" (binary_coded_decimal (-1))
printfn "%s" (binary_coded_decimal (0))
printfn "%s" (binary_coded_decimal (3))
printfn "%s" (binary_coded_decimal (2))
printfn "%s" (binary_coded_decimal (12))
printfn "%s" (binary_coded_decimal (987))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
