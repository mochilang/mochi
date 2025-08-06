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

let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (ch: string) =
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
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt (message: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable message = message
    try
        for key in 0 .. ((String.length (LETTERS)) - 1) do
            let mutable translated: string = ""
            for i in 0 .. ((String.length (message)) - 1) do
                let symbol: string = _substring message i (i + 1)
                let idx: int = index_of (LETTERS) (symbol)
                if idx <> (0 - 1) then
                    let mutable num: int = idx - key
                    if num < 0 then
                        num <- num + (String.length (LETTERS))
                    translated <- translated + (_substring LETTERS num (num + 1))
                else
                    translated <- translated + symbol
            printfn "%s" ((("Decryption using Key #" + (_str (key))) + ": ") + translated)
        __ret
    with
        | Return -> __ret
decrypt ("TMDETUX PMDVU")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
