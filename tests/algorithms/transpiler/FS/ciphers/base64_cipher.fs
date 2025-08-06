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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let B64_CHARSET: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let rec to_binary (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable num: int = n
        let mutable res: string = ""
        while num > 0 do
            let bit: int = ((num % 2 + 2) % 2)
            res <- (_str (bit)) + res
            num <- num / 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and zfill (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable res: string = s
        let mutable pad: int = width - (String.length (s))
        while pad > 0 do
            res <- "0" + res
            pad <- pad - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and from_binary (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable i: int = 0
        let mutable result: int = 0
        while i < (String.length (s)) do
            result <- result * 2
            if (_substring s i (i + 1)) = "1" then
                result <- result + 1
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (ch: string) (times: int) =
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
and char_index (s: string) (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = c then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and base64_encode (data: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let mutable bits: string = ""
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            bits <- bits + (zfill (to_binary (_idx data (i))) (8))
            i <- i + 1
        let mutable pad_bits: int = 0
        if ((((String.length (bits)) % 6 + 6) % 6)) <> 0 then
            pad_bits <- 6 - ((((String.length (bits)) % 6 + 6) % 6))
            bits <- bits + (repeat ("0") (pad_bits))
        let mutable j: int = 0
        let mutable encoded: string = ""
        while j < (String.length (bits)) do
            let chunk: string = _substring bits j (j + 6)
            let idx: int = from_binary (chunk)
            encoded <- encoded + (_substring B64_CHARSET idx (idx + 1))
            j <- j + 6
        let mutable pad: int = pad_bits / 2
        while pad > 0 do
            encoded <- encoded + "="
            pad <- pad - 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
and base64_decode (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable padding: int = 0
        let mutable ``end``: int = String.length (s)
        while (``end`` > 0) && ((_substring s (``end`` - 1) ``end``) = "=") do
            padding <- padding + 1
            ``end`` <- ``end`` - 1
        let mutable bits: string = ""
        let mutable k: int = 0
        while k < ``end`` do
            let c: string = _substring s k (k + 1)
            let idx: int = char_index (B64_CHARSET) (c)
            bits <- bits + (zfill (to_binary (idx)) (6))
            k <- k + 1
        if padding > 0 then
            bits <- _substring bits 0 ((String.length (bits)) - (padding * 2))
        let mutable bytes: int array = [||]
        let mutable m: int = 0
        while m < (String.length (bits)) do
            let byte: int = from_binary (_substring bits m (m + 8))
            bytes <- Array.append bytes [|byte|]
            m <- m + 8
        __ret <- bytes
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: int array = [|77; 111; 99; 104; 105|]
        let mutable encoded: string = base64_encode (data)
        printfn "%s" (encoded)
        json (base64_decode (encoded))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
