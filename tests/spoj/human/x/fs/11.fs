// Generated 2025-08-26 14:25 +0700

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

let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
open System.Collections.Generic

open System

let rec parseIntStr (str: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable str = str
    try
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        let mutable i: int64 = int64 0
        let mutable n: int64 = int64 0
        while i < (int64 (String.length (str))) do
            n <- (n * (int64 10)) + (int64 (_dictGet digits ((string (_substring str (int i) (int (i + (int64 1))))))))
            i <- i + (int64 1)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and trailingZeros (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable count: int64 = int64 0
        let mutable d: int64 = int64 5
        while d <= n do
            count <- count + (_floordiv64 (int64 n) (int64 d))
            d <- d * (int64 5)
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tStr: string = _readLine()
        if tStr = "" then
            __ret <- ()
            raise Return
        let t: int64 = parseIntStr (tStr)
        for _ in 0 .. ((int t) - 1) do
            let nStr: string = _readLine()
            let mutable n: int64 = parseIntStr (nStr)
            ignore (printfn "%d" (trailingZeros (int64 n)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
