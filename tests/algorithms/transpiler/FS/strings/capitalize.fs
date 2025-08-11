// Generated 2025-08-11 17:23 +0700

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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let lowercase: string = "abcdefghijklmnopqrstuvwxyz"
let uppercase: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (c: string) =
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
and capitalize (sentence: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sentence = sentence
    try
        if (String.length (sentence)) = 0 then
            __ret <- ""
            raise Return
        let first: string = _substring sentence 0 1
        let idx: int = index_of (lowercase) (first)
        let capital: string = if idx >= 0 then (_substring uppercase idx (idx + 1)) else first
        __ret <- capital + (_substring sentence 1 (String.length (sentence)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (capitalize ("hello world"))
printfn "%s" (capitalize ("123 hello world"))
printfn "%s" (capitalize (" hello world"))
printfn "%s" (capitalize ("a"))
printfn "%s" (capitalize (""))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
