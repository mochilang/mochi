// Generated 2025-07-26 04:38 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((string (str.[0])) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- (n * 10) + (int (digits.[(string (str.[i]))] |> unbox<int>))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = indexOf upper ch
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- indexOf lower ch
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        if (ch >= "0") && (ch <= "9") then
            __ret <- 48 + (int (parseIntStr ch))
            raise Return
        if ch = "+" then
            __ret <- 43
            raise Return
        if ch = "/" then
            __ret <- 47
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "=" then
            __ret <- 61
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 65) && (n < 91) then
            __ret <- upper.Substring(n - 65, (n - 64) - (n - 65))
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- lower.Substring(n - 97, (n - 96) - (n - 97))
            raise Return
        if (n >= 48) && (n < 58) then
            let digits: string = "0123456789"
            __ret <- digits.Substring(n - 48, (n - 47) - (n - 48))
            raise Return
        if n = 43 then
            __ret <- "+"
            raise Return
        if n = 47 then
            __ret <- "/"
            raise Return
        if n = 32 then
            __ret <- " "
            raise Return
        if n = 61 then
            __ret <- "="
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
let rec toBinary (n: int) (bits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable bits = bits
    try
        let mutable b: string = ""
        let mutable ``val``: int = n
        let mutable i: int = 0
        while i < bits do
            b <- (string (((``val`` % 2 + 2) % 2))) + b
            ``val`` <- int (``val`` / 2)
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
let rec binToInt (bits: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bits = bits
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length bits) do
            n <- (n * 2) + (int (parseIntStr (bits.Substring(i, (i + 1) - i))))
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec base64Encode (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        let alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        let mutable bin: string = ""
        for ch in text do
            bin <- bin + (unbox<string> (toBinary (int (ord (unbox<string> ch))) 8))
        while ((((String.length bin) % 6 + 6) % 6)) <> 0 do
            bin <- bin + "0"
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length bin) do
            let chunk: string = bin.Substring(i, (i + 6) - i)
            let ``val``: int = binToInt chunk
            out <- out + (alphabet.Substring(``val``, (``val`` + 1) - ``val``))
            i <- i + 6
        let pad: int = (((3 - ((((String.length text) % 3 + 3) % 3))) % 3 + 3) % 3)
        if pad = 1 then
            out <- (out.Substring(0, ((String.length out) - 1) - 0)) + "="
        if pad = 2 then
            out <- (out.Substring(0, ((String.length out) - 2) - 0)) + "=="
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec base64Decode (enc: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable enc = enc
    try
        let alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        let mutable bin: string = ""
        let mutable i: int = 0
        try
            while i < (String.length enc) do
                let ch: string = string (enc.[i])
                if ch = "=" then
                    raise Break
                let idx: int = indexOf alphabet ch
                bin <- bin + (unbox<string> (toBinary idx 6))
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        let mutable out: string = ""
        i <- 0
        while (i + 8) <= (String.length bin) do
            let chunk: string = bin.Substring(i, (i + 8) - i)
            let ``val``: int = binToInt chunk
            out <- out + (unbox<string> (chr ``val``))
            i <- i + 8
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let msg: string = "Rosetta Code Base64 decode data task"
printfn "%s" ("Original : " + msg)
let enc: string = base64Encode msg
printfn "%s" ("\nEncoded  : " + enc)
let dec: string = base64Decode enc
printfn "%s" ("\nDecoded  : " + dec)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
