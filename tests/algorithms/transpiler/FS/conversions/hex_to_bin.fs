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

let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
let rec trim_spaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        let mutable ``end``: int = String.length (s)
        while (start < ``end``) && ((_substring s start (start + 1)) = " ") do
            start <- start + 1
        while (``end`` > start) && ((_substring s (``end`` - 1) ``end``) = " ") do
            ``end`` <- ``end`` - 1
        __ret <- _substring s start ``end``
        raise Return
        __ret
    with
        | Return -> __ret
let rec hex_digit_value (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "0" then
            __ret <- 0
            raise Return
        if ch = "1" then
            __ret <- 1
            raise Return
        if ch = "2" then
            __ret <- 2
            raise Return
        if ch = "3" then
            __ret <- 3
            raise Return
        if ch = "4" then
            __ret <- 4
            raise Return
        if ch = "5" then
            __ret <- 5
            raise Return
        if ch = "6" then
            __ret <- 6
            raise Return
        if ch = "7" then
            __ret <- 7
            raise Return
        if ch = "8" then
            __ret <- 8
            raise Return
        if ch = "9" then
            __ret <- 9
            raise Return
        if (ch = "a") || (ch = "A") then
            __ret <- 10
            raise Return
        if (ch = "b") || (ch = "B") then
            __ret <- 11
            raise Return
        if (ch = "c") || (ch = "C") then
            __ret <- 12
            raise Return
        if (ch = "d") || (ch = "D") then
            __ret <- 13
            raise Return
        if (ch = "e") || (ch = "E") then
            __ret <- 14
            raise Return
        if (ch = "f") || (ch = "F") then
            __ret <- 15
            raise Return
        panic ("Invalid value was passed to the function")
        __ret
    with
        | Return -> __ret
let rec hex_to_bin (hex_num: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable hex_num = hex_num
    try
        let trimmed: string = trim_spaces (hex_num)
        if (String.length (trimmed)) = 0 then
            panic ("No value was passed to the function")
        let mutable s: string = trimmed
        let mutable is_negative: bool = false
        if (_substring s 0 1) = "-" then
            is_negative <- true
            s <- _substring s 1 (String.length (s))
        let mutable int_num: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            let ``val``: int = hex_digit_value (ch)
            int_num <- (int_num * 16) + ``val``
            i <- i + 1
        let mutable bin_str: string = ""
        let mutable n: int = int_num
        if n = 0 then
            bin_str <- "0"
        while n > 0 do
            bin_str <- (_str (((n % 2 + 2) % 2))) + bin_str
            n <- n / 2
        let mutable result: int = int bin_str
        if is_negative then
            result <- -result
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (hex_to_bin ("AC")))
printfn "%s" (_str (hex_to_bin ("9A4")))
printfn "%s" (_str (hex_to_bin ("   12f   ")))
printfn "%s" (_str (hex_to_bin ("FfFf")))
printfn "%s" (_str (hex_to_bin ("-fFfF")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
