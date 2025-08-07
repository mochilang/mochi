// Generated 2025-08-07 10:31 +0700

exception Break
exception Continue

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
let rec trim (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        try
            while start < (String.length (s)) do
                try
                    let ch: string = s.Substring(start, (start + 1) - start)
                    if (((ch <> " ") && (ch <> "\n")) && (ch <> "\t")) && (ch <> "\r") then
                        raise Break
                    start <- start + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable ``end``: int = String.length (s)
        try
            while ``end`` > start do
                try
                    let ch: string = s.Substring(``end`` - 1, ``end`` - (``end`` - 1))
                    if (((ch <> " ") && (ch <> "\n")) && (ch <> "\t")) && (ch <> "\r") then
                        raise Break
                    ``end`` <- ``end`` - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- _substring s start ``end``
        raise Return
        __ret
    with
        | Return -> __ret
let rec bin_to_decimal (bin_string: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bin_string = bin_string
    try
        let trimmed: string = trim (bin_string)
        if trimmed = "" then
            failwith ("Empty string was passed to the function")
        let mutable is_negative: bool = false
        let mutable s: string = trimmed
        if (s.Substring(0, 1 - 0)) = "-" then
            is_negative <- true
            s <- _substring s 1 (String.length (s))
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = s.Substring(i, (i + 1) - i)
            if (c <> "0") && (c <> "1") then
                failwith ("Non-binary value was passed to the function")
            i <- i + 1
        let mutable decimal_number: int = 0
        i <- 0
        while i < (String.length (s)) do
            let c: string = s.Substring(i, (i + 1) - i)
            let digit: int = int c
            decimal_number <- (2 * decimal_number) + digit
            i <- i + 1
        if is_negative then
            __ret <- -decimal_number
            raise Return
        __ret <- decimal_number
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (bin_to_decimal ("101")))
printfn "%s" (_str (bin_to_decimal (" 1010   ")))
printfn "%s" (_str (bin_to_decimal ("-11101")))
printfn "%s" (_str (bin_to_decimal ("0")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
