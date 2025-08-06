// Generated 2025-08-06 21:33 +0700

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
let rec get_reverse_bit_string (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        let mutable bit_string: string = ""
        let mutable n: int = number
        let mutable i: int = 0
        while i < 32 do
            bit_string <- bit_string + (_str (((n % 2 + 2) % 2)))
            n <- n / 2
            i <- i + 1
        __ret <- bit_string
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_bit (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        if number < 0 then
            failwith ("the value of input must be positive")
        let mutable n: int = number
        let mutable result: int = 0
        let mutable i: int = 1
        while i <= 32 do
            result <- result * 2
            let end_bit: int = ((n % 2 + 2) % 2)
            n <- n / 2
            result <- result + end_bit
            i <- i + 1
        __ret <- get_reverse_bit_string (result)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (reverse_bit (25))
printfn "%s" (reverse_bit (37))
printfn "%s" (reverse_bit (21))
printfn "%s" (reverse_bit (58))
printfn "%s" (reverse_bit (0))
printfn "%s" (reverse_bit (256))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
