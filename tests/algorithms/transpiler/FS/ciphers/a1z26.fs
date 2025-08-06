// Generated 2025-08-06 23:19 +0700

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
open System

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and charToNum (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let letters: string = "abcdefghijklmnopqrstuvwxyz"
        let idx: int = indexOf (letters) (ch)
        if idx >= 0 then
            __ret <- idx + 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and numToChar (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let letters: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 1) && (n <= 26) then
            __ret <- _substring letters (n - 1) n
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
and encode (plain: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable plain = plain
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (String.length (plain)) do
            let ch: string = (_substring plain i (i + 1)).ToLower()
            let ``val``: int = charToNum (ch)
            if ``val`` > 0 then
                res <- Array.append res [|``val``|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and decode (encoded: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable encoded = encoded
    try
        let mutable out: string = ""
        for n in encoded do
            out <- out + (numToChar (n))
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("-> ")
        let text: string = (System.Console.ReadLine()).ToLower()
        let enc: int array = encode (text)
        printfn "%s" ("Encoded: " + (_str (enc)))
        printfn "%s" ("Decoded: " + (decode (enc)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
