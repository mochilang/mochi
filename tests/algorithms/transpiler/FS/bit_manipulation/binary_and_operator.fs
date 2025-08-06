// Generated 2025-08-06 20:48 +0700

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
        let mutable pad: int = width - (String.length(s))
        while pad > 0 do
            res <- "0" + res
            pad <- pad - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and binary_and (a: int) (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        if (a < 0) || (b < 0) then
            failwith ("the value of both inputs must be positive")
        let a_bin: string = to_binary (a)
        let b_bin: string = to_binary (b)
        let mutable max_len: int = String.length(a_bin)
        if (String.length(b_bin)) > max_len then
            max_len <- String.length(b_bin)
        let a_pad: string = zfill (a_bin) (max_len)
        let b_pad: string = zfill (b_bin) (max_len)
        let mutable i: int = 0
        let mutable res: string = ""
        while i < max_len do
            if ((string (a_pad.[i])) = "1") && ((string (b_pad.[i])) = "1") then
                res <- res + "1"
            else
                res <- res + "0"
            i <- i + 1
        __ret <- "0b" + res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (binary_and (25) (32))
printfn "%s" (binary_and (37) (50))
printfn "%s" (binary_and (21) (30))
printfn "%s" (binary_and (58) (73))
printfn "%s" (binary_and (0) (255))
printfn "%s" (binary_and (256) (256))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
