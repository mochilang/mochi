// Generated 2025-08-11 15:32 +0700

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

let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec create_ngram (sentence: string) (ngram_size: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable sentence = sentence
    let mutable ngram_size = ngram_size
    try
        let mutable res: string array = Array.empty<string>
        let bound: int = ((String.length (sentence)) - ngram_size) + 1
        if bound <= 0 then
            __ret <- res
            raise Return
        let mutable i: int = 0
        while i < bound do
            res <- Array.append res [|(_substring sentence i (i + ngram_size))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let example1: string = "I am a sentence"
printfn "%s" (_repr (create_ngram (example1) (2)))
let example2: string = "I am an NLPer"
printfn "%s" (_repr (create_ngram (example2) (2)))
let example3: string = "This is short"
printfn "%s" (_repr (create_ngram (example3) (50)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
