// Generated 2025-08-06 21:04 +0700

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
let rec repeat_char (ch: string) (times: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable times = times
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < times do
            res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and to_binary (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable res: string = ""
        let mutable v: int = n
        while v > 0 do
            res <- (_str (((v % 2 + 2) % 2))) + res
            v <- v / 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < exp do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and twos_complement (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        if number > 0 then
            failwith ("input must be a negative integer")
        if number = 0 then
            __ret <- "0b0"
            raise Return
        let abs_number: int = if number < 0 then (-number) else number
        let binary_number_length: int = String.length (to_binary (abs_number))
        let complement_value: int = (pow2 (binary_number_length)) - abs_number
        let complement_binary: string = to_binary (complement_value)
        let padding: string = repeat_char ("0") (binary_number_length - (String.length (complement_binary)))
        let twos_complement_number: string = ("1" + padding) + complement_binary
        __ret <- "0b" + twos_complement_number
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (twos_complement (0))
printfn "%s" (twos_complement (-1))
printfn "%s" (twos_complement (-5))
printfn "%s" (twos_complement (-17))
printfn "%s" (twos_complement (-207))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
