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
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
and trim_spaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        let mutable ``end``: int = (String.length (s)) - 1
        while (start <= ``end``) && ((_substring s start (start + 1)) = " ") do
            start <- start + 1
        while (``end`` >= start) && ((_substring s ``end`` (``end`` + 1)) = " ") do
            ``end`` <- ``end`` - 1
        if start > ``end`` then
            __ret <- ""
            raise Return
        __ret <- _substring s start (``end`` + 1)
        raise Return
        __ret
    with
        | Return -> __ret
and char_to_digit (ch: string) =
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
        panic ("Non-octal value was passed to the function")
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and oct_to_decimal (oct_string: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable oct_string = oct_string
    try
        let mutable s: string = trim_spaces (oct_string)
        if (String.length (s)) = 0 then
            panic ("Empty string was passed to the function")
            __ret <- 0
            raise Return
        let mutable is_negative: bool = false
        if (_substring s 0 1) = "-" then
            is_negative <- true
            s <- _substring s 1 (String.length (s))
        if (String.length (s)) = 0 then
            panic ("Non-octal value was passed to the function")
            __ret <- 0
            raise Return
        let mutable decimal_number: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            let digit: int = char_to_digit (ch)
            decimal_number <- (8 * decimal_number) + digit
            i <- i + 1
        if is_negative then
            decimal_number <- -decimal_number
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
        printfn "%s" (_str (oct_to_decimal ("1")))
        printfn "%s" (_str (oct_to_decimal ("-1")))
        printfn "%s" (_str (oct_to_decimal ("12")))
        printfn "%s" (_str (oct_to_decimal (" 12   ")))
        printfn "%s" (_str (oct_to_decimal ("-45")))
        printfn "%s" (_str (oct_to_decimal ("0")))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
