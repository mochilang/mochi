// Generated 2025-07-27 15:57 +0700

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
let epsilon: float = 0.000000000000001
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < n do
            r <- r * 10.0
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec formatFloat (f: float) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    let mutable prec = prec
    try
        let scale: float = pow10 prec
        let scaled: float = (f * scale) + 0.5
        let mutable n: int = unbox<int> scaled
        let mutable digits: string = string n
        while (String.length digits) <= prec do
            digits <- "0" + digits
        let intPart: string = _substring digits 0 ((String.length digits) - prec)
        let fracPart: string = _substring digits ((String.length digits) - prec) (String.length digits)
        __ret <- (intPart + ".") + fracPart
        raise Return
        __ret
    with
        | Return -> __ret
let mutable factval: int = 1
let mutable e: float = 2.0
let mutable n: int = 2
let mutable term: float = 1.0
try
    while true do
        factval <- factval * n
        n <- n + 1
        term <- 1.0 / (unbox<float> factval)
        e <- e + term
        if (unbox<float> (absf term)) < epsilon then
            raise Break
with
| Break -> ()
| Continue -> ()
printfn "%s" ("e = " + (unbox<string> (formatFloat e 15)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
