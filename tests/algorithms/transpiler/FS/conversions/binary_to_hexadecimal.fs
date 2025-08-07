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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec strip_spaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        let mutable ``end``: int = (String.length (s)) - 1
        while (start < (String.length (s))) && ((string (s.[start])) = " ") do
            start <- start + 1
        while (``end`` >= start) && ((string (s.[``end``])) = " ") do
            ``end`` <- ``end`` - 1
        let mutable res: string = ""
        let mutable i: int = start
        while i <= ``end`` do
            res <- res + (string (s.[i]))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec repeat_char (ch: string) (count: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable count = count
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < count do
            res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec slice (s: string) (start: int) (``end``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable res: string = ""
        let mutable i: int = start
        while i < ``end`` do
            res <- res + (string (s.[i]))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bits_to_int (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (bits)) do
            value <- value * 2
            if (string (bits.[i])) = "1" then
                value <- value + 1
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec bin_to_hexadecimal (binary_str: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable binary_str = binary_str
    try
        let mutable s: string = strip_spaces (binary_str)
        if (String.length (s)) = 0 then
            failwith ("Empty string was passed to the function")
        let mutable is_negative: bool = false
        if (string (s.[0])) = "-" then
            is_negative <- true
            s <- slice (s) (1) (String.length (s))
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c <> "0") && (c <> "1") then
                failwith ("Non-binary value was passed to the function")
            i <- i + 1
        let groups: int = ((String.length (s)) / 4) + 1
        let pad_len: int = (groups * 4) - (String.length (s))
        s <- (repeat_char ("0") (pad_len)) + s
        let digits: string = "0123456789abcdef"
        let mutable res: string = "0x"
        let mutable j: int = 0
        while j < (String.length (s)) do
            let chunk: string = slice (s) (j) (j + 4)
            let ``val``: int = bits_to_int (chunk)
            res <- res + (string (digits.[``val``]))
            j <- j + 4
        if is_negative then
            __ret <- "-" + res
            raise Return
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (bin_to_hexadecimal ("101011111"))
printfn "%s" (bin_to_hexadecimal (" 1010   "))
printfn "%s" (bin_to_hexadecimal ("-11101"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
