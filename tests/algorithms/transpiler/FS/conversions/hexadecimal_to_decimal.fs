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
let rec strip (s: string) =
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
and hex_digit_value (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        if c = "0" then
            __ret <- 0
            raise Return
        if c = "1" then
            __ret <- 1
            raise Return
        if c = "2" then
            __ret <- 2
            raise Return
        if c = "3" then
            __ret <- 3
            raise Return
        if c = "4" then
            __ret <- 4
            raise Return
        if c = "5" then
            __ret <- 5
            raise Return
        if c = "6" then
            __ret <- 6
            raise Return
        if c = "7" then
            __ret <- 7
            raise Return
        if c = "8" then
            __ret <- 8
            raise Return
        if c = "9" then
            __ret <- 9
            raise Return
        if (c = "a") || (c = "A") then
            __ret <- 10
            raise Return
        if (c = "b") || (c = "B") then
            __ret <- 11
            raise Return
        if (c = "c") || (c = "C") then
            __ret <- 12
            raise Return
        if (c = "d") || (c = "D") then
            __ret <- 13
            raise Return
        if (c = "e") || (c = "E") then
            __ret <- 14
            raise Return
        if (c = "f") || (c = "F") then
            __ret <- 15
            raise Return
        printfn "%s" ("Non-hexadecimal value was passed to the function")
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and hex_to_decimal (hex_string: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable hex_string = hex_string
    try
        let mutable s: string = strip (hex_string)
        if (String.length (s)) = 0 then
            printfn "%s" ("Empty string was passed to the function")
            __ret <- 0
            raise Return
        let mutable is_negative: bool = false
        if (_substring s 0 1) = "-" then
            is_negative <- true
            s <- _substring s 1 (String.length (s))
        let mutable decimal_number: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = _substring s i (i + 1)
            let value: int = hex_digit_value (c)
            decimal_number <- (16 * decimal_number) + value
            i <- i + 1
        if is_negative then
            __ret <- -decimal_number
            raise Return
        __ret <- decimal_number
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (hex_to_decimal ("a")))
        printfn "%s" (_str (hex_to_decimal ("12f")))
        printfn "%s" (_str (hex_to_decimal ("   12f   ")))
        printfn "%s" (_str (hex_to_decimal ("FfFf")))
        printfn "%s" (_str (hex_to_decimal ("-Ff")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
