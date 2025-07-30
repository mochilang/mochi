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
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        let mutable d: int = 3
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let digits: int array = [|9; 8; 7; 6; 5; 4; 3; 2; 1|]
let rec gen (idx: int) (cur: int) (used: bool) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable idx = idx
    let mutable cur = cur
    let mutable used = used
    try
        if idx = (Seq.length digits) then
            if used && (unbox<bool> (isPrime cur)) then
                __ret <- unbox<int array> [|cur|]
                raise Return
            __ret <- Array.empty<int>
            raise Return
        let ``with``: int array = gen (idx + 1) ((cur * 10) + (digits.[idx])) true
        let without: int array = gen (idx + 1) cur used
        __ret <- unbox<int array> (``with`` union without)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable primes: int array = gen 0 0 false
let rec pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (("There are " + (string (Seq.length primes))) + " descending primes, namely:")
let mutable i: int = 0
let mutable line: string = ""
while i < (Seq.length primes) do
    line <- (line + (unbox<string> (pad (primes.[i]) 8))) + " "
    if ((((i + 1) % 10 + 10) % 10)) = 0 then
        printfn "%s" (_substring line 0 ((String.length line) - 1))
        line <- ""
    i <- i + 1
if (String.length line) > 0 then
    printfn "%s" (_substring line 0 ((String.length line) - 1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
