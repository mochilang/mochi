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
let ascii85_chars: string = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu"
let rec indexOf (s: string) (ch: string) =
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
        let idx: int = indexOf (ascii85_chars) (ch)
        if idx >= 0 then
            __ret <- 33 + idx
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- if (n >= 33) && (n <= 117) then (ascii85_chars.Substring(n - 33, (n - 32) - (n - 33))) else "?"
        raise Return
        __ret
    with
        | Return -> __ret
and to_binary (n: int) (bits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable bits = bits
    try
        let mutable b: string = ""
        let mutable ``val``: int = n
        while ``val`` > 0 do
            b <- (_str (((``val`` % 2 + 2) % 2))) + b
            ``val`` <- ``val`` / 2
        while (String.length (b)) < bits do
            b <- "0" + b
        if (String.length (b)) = 0 then
            b <- "0"
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and bin_to_int (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length (bits)) do
            if (string (bits.[i])) = "1" then
                n <- (n * 2) + 1
            else
                n <- n * 2
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and reverse (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = (String.length (s)) - 1
        while i >= 0 do
            res <- res + (string (s.[i]))
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and base10_to_85 (d: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    try
        __ret <- if d > 0 then ((chr ((((d % 85 + 85) % 85)) + 33)) + (base10_to_85 (d / 85))) else ""
        raise Return
        __ret
    with
        | Return -> __ret
and base85_to_10 (digits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable digits = digits
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (digits)) do
            value <- (value * 85) + ((ord (string (digits.[i]))) - 33)
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and ascii85_encode (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let mutable binary_data: string = ""
        for ch in Seq.map string (data) do
            binary_data <- binary_data + (to_binary (ord (ch)) (8))
        let mutable null_values: int = ((32 * (((String.length (binary_data)) / 32) + 1)) - (String.length (binary_data))) / 8
        let mutable total_bits: int = 32 * (((String.length (binary_data)) / 32) + 1)
        while (String.length (binary_data)) < total_bits do
            binary_data <- binary_data + "0"
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (String.length (binary_data)) do
            let chunk_bits: string = binary_data.Substring(i, (i + 32) - i)
            let chunk_val: int = bin_to_int (chunk_bits)
            let encoded: string = reverse (base10_to_85 (chunk_val))
            result <- result + encoded
            i <- i + 32
        if (((null_values % 4 + 4) % 4)) <> 0 then
            result <- result.Substring(0, ((String.length (result)) - null_values) - 0)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and ascii85_decode (data: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let mutable null_values: int = (5 * (((String.length (data)) / 5) + 1)) - (String.length (data))
        let mutable binary_data: string = data
        let mutable i: int = 0
        while i < null_values do
            binary_data <- binary_data + "u"
            i <- i + 1
        let mutable result: string = ""
        i <- 0
        while i < (String.length (binary_data)) do
            let chunk: string = binary_data.Substring(i, (i + 5) - i)
            let mutable value: int = base85_to_10 (chunk)
            let bits: string = to_binary (value) (32)
            let mutable j: int = 0
            while j < 32 do
                let byte_bits: string = bits.Substring(j, (j + 8) - j)
                let c: string = chr (bin_to_int (byte_bits))
                result <- result + c
                j <- j + 8
            i <- i + 5
        let mutable trim: int = null_values
        if (((null_values % 5 + 5) % 5)) = 0 then
            trim <- null_values - 1
        __ret <- result.Substring(0, ((String.length (result)) - trim) - 0)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (ascii85_encode (""))
printfn "%s" (ascii85_encode ("12345"))
printfn "%s" (ascii85_encode ("base 85"))
printfn "%s" (ascii85_decode (""))
printfn "%s" (ascii85_decode ("0etOA2#"))
printfn "%s" (ascii85_decode ("@UX=h+?24"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
