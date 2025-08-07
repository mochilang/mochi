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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let abit: int = ((x % 2 + 2) % 2)
            let bbit: int = ((y % 2 + 2) % 2)
            if abit <> bbit then
                res <- res + bit
            x <- x / 2
            y <- y / 2
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let ascii: string = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (ascii)) do
            if (ascii.Substring(i, (i + 1) - i)) = ch then
                __ret <- 32 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- if (n >= 32) && (n < 127) then (ascii.Substring(n - 32, (n - 31) - (n - 32))) else ""
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalize_key (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable key = key
    try
        let mutable k: int = key
        if k = 0 then
            k <- 1
        k <- ((k % 256 + 256) % 256)
        if k < 0 then
            k <- k + 256
        __ret <- k
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt (content: string) (key: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable content = content
    let mutable key = key
    try
        let mutable k: int = normalize_key (key)
        let mutable result: string array = [||]
        let mutable i: int = 0
        while i < (String.length (content)) do
            let c: int = ord (content.Substring(i, (i + 1) - i))
            let e: int = xor (c) (k)
            result <- Array.append result [|chr (e)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt_string (content: string) (key: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable content = content
    let mutable key = key
    try
        let chars: string array = encrypt (content) (key)
        let mutable out: string = ""
        for ch in Seq.map string (chars) do
            out <- out + ch
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let sample: string = "hallo welt"
let enc: string = encrypt_string (sample) (1)
let dec: string = encrypt_string (enc) (1)
printfn "%s" (_str (encrypt (sample) (1)))
printfn "%s" (enc)
printfn "%s" (dec)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
