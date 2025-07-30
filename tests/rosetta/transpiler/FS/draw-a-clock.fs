// Generated 2025-07-31 00:10 +0700

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
open System

let rec pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < exp do
            r <- r * 2
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec bin (n: int) (digits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable digits = digits
    try
        let mutable s: string = ""
        let mutable i: int = digits - 1
        while i >= 0 do
            let p: int = pow2 i
            if n >= p then
                s <- s + "x"
                n <- n - p
            else
                s <- s + " "
            if i > 0 then
                s <- s + "|"
            i <- i - 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let t: int = (int (_now())) / 1000000000
let sec: int = ((t % 60 + 60) % 60)
let mins: int = t / 60
let min: int = ((mins % 60 + 60) % 60)
let hour: int = (((mins / 60) % 24 + 24) % 24)
printfn "%s" (bin hour 8)
printfn "%s" ""
printfn "%s" (bin min 8)
printfn "%s" ""
let mutable xs: string = ""
let mutable i: int = 0
while i < sec do
    xs <- xs + "x"
    i <- i + 1
let mutable out: string = ""
let mutable j: int = 0
while j < (String.length xs) do
    out <- out + (_substring xs j (j + 1))
    if (((((j + 1) % 5 + 5) % 5)) = 0) && ((j + 1) < (String.length xs)) then
        out <- out + "|"
    j <- j + 1
printfn "%s" out
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
