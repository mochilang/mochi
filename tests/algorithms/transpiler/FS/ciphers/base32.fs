// Generated 2025-08-06 23:33 +0700

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
let B32_CHARSET: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
let rec indexOfChar (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let digits: string = "0123456789"
        let mutable idx: int = indexOfChar (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- indexOfChar (lower) (ch)
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        idx <- indexOfChar (digits) (ch)
        if idx >= 0 then
            __ret <- 48 + idx
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "!" then
            __ret <- 33
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and chr (code: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable code = code
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let digits: string = "0123456789"
        if code = 32 then
            __ret <- " "
            raise Return
        if code = 33 then
            __ret <- "!"
            raise Return
        let mutable idx: int = code - 65
        if (idx >= 0) && (idx < (String.length (upper))) then
            __ret <- string (upper.[idx])
            raise Return
        idx <- code - 97
        if (idx >= 0) && (idx < (String.length (lower))) then
            __ret <- string (lower.[idx])
            raise Return
        idx <- code - 48
        if (idx >= 0) && (idx < (String.length (digits))) then
            __ret <- string (digits.[idx])
            raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < n do
            out <- out + s
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and to_binary (n: int) (bits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable bits = bits
    try
        let mutable v: int = n
        let mutable out: string = ""
        let mutable i: int = 0
        while i < bits do
            out <- (_str (((v % 2 + 2) % 2))) + out
            v <- v / 2
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and binary_to_int (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length (bits)) do
            n <- n * 2
            if (string (bits.[i])) = "1" then
                n <- n + 1
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and base32_encode (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let mutable binary_data: string = ""
        let mutable i: int = 0
        while i < (String.length (data)) do
            binary_data <- binary_data + (to_binary (ord (string (data.[i]))) (8))
            i <- i + 1
        let remainder: int = (((String.length (binary_data)) % 5 + 5) % 5)
        if remainder <> 0 then
            binary_data <- binary_data + (repeat ("0") (5 - remainder))
        let mutable b32_result: string = ""
        let mutable j: int = 0
        while j < (String.length (binary_data)) do
            let chunk: string = binary_data.Substring(j, (j + 5) - j)
            let index: int = binary_to_int (chunk)
            b32_result <- b32_result + (string (B32_CHARSET.[index]))
            j <- j + 5
        let rem: int = (((String.length (b32_result)) % 8 + 8) % 8)
        if rem <> 0 then
            b32_result <- b32_result + (repeat ("=") (8 - rem))
        __ret <- b32_result
        raise Return
        __ret
    with
        | Return -> __ret
and base32_decode (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let mutable clean: string = ""
        let mutable i: int = 0
        while i < (String.length (data)) do
            let ch: string = string (data.[i])
            if ch <> "=" then
                clean <- clean + ch
            i <- i + 1
        let mutable binary_chunks: string = ""
        i <- 0
        while i < (String.length (clean)) do
            let mutable idx: int = indexOfChar (B32_CHARSET) (string (clean.[i]))
            binary_chunks <- binary_chunks + (to_binary (idx) (5))
            i <- i + 1
        let mutable result: string = ""
        let mutable j: int = 0
        while (j + 8) <= (String.length (binary_chunks)) do
            let byte_bits: string = binary_chunks.Substring(j, (j + 8) - j)
            let code: int = binary_to_int (byte_bits)
            result <- result + (chr (code))
            j <- j + 8
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (base32_encode ("Hello World!"))
printfn "%s" (base32_encode ("123456"))
printfn "%s" (base32_encode ("some long complex string"))
printfn "%s" (base32_decode ("JBSWY3DPEBLW64TMMQQQ===="))
printfn "%s" (base32_decode ("GEZDGNBVGY======"))
printfn "%s" (base32_decode ("ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
