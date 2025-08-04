// Generated 2025-08-04 22:35 +0700

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

open System

module math =
    let pi: float = System.Math.PI
    let e: float = System.Math.E
    let rec sqrt x =
        let mutable __ret : float = Unchecked.defaultof<float>
        let mutable x = x
        try
            __ret <- System.Math.Sqrt(x)
            raise Return
            __ret
        with
            | Return -> __ret
    let rec pow x y =
        let mutable __ret : float = Unchecked.defaultof<float>
        let mutable x = x
        let mutable y = y
        try
            __ret <- System.Math.Pow(x, y)
            raise Return
            __ret
        with
            | Return -> __ret
    let rec sin x =
        let mutable __ret : float = Unchecked.defaultof<float>
        let mutable x = x
        try
            __ret <- System.Math.Sin(x)
            raise Return
            __ret
        with
            | Return -> __ret
    let rec log x =
        let mutable __ret : float = Unchecked.defaultof<float>
        let mutable x = x
        try
            __ret <- System.Math.Log(x)
            raise Return
            __ret
        with
            | Return -> __ret

let rec entropy (s: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    try
        let mutable counts: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length(s)) do
            let ch: string = _substring s i (i + 1)
            if counts.ContainsKey(ch) then
                counts.[ch] <- (counts.[(string (ch))]) + 1
            else
                counts.[ch] <- 1
            i <- i + 1
        let mutable hm: float = 0.0
        for k in counts.Keys do
            let c: float = System.Convert.ToDouble (counts.[(string (k))])
            hm <- float (hm + (float (c * (float ((float (math.log(c))) / (float (math.log(2.0))))))))
        let l: float = float (String.length(s))
        __ret <- float ((float ((float (math.log(l))) / (float (math.log(2.0))))) - (hm / l))
        raise Return
        __ret
    with
        | Return -> __ret
and fibonacciWord (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable a: string = "1"
        let mutable b: string = "0"
        let mutable i: int = 1
        while i < n do
            let tmp: string = b
            b <- b + a
            a <- tmp
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (((pad ("N") (3)) + (pad ("Length") (9))) + "  Entropy      Word")
        let mutable n: int = 1
        while n < 10 do
            let mutable s: string = fibonacciWord (n)
            printfn "%s" ((((((pad (string (n)) (3)) + (pad (string (String.length(s))) (9))) + "  ") + (fmt (entropy (s)))) + "  ") + s)
            n <- n + 1
        while n <= 37 do
            let mutable s: string = fibonacciWord (n)
            printfn "%s" ((((pad (string (n)) (3)) + (pad (string (String.length(s))) (9))) + "  ") + (fmt (entropy (s))))
            n <- n + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable t: string = s
        while (String.length(t)) < w do
            t <- " " + t
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and fmt (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (floorf ((x * 100000000.0) + 0.5)) / 100000000.0
        let mutable s: string = string (y)
        let mutable dot: int = s.IndexOf(".")
        if dot = (0 - 1) then
            s <- s + ".00000000"
        else
            let mutable d: int = ((String.length(s)) - dot) - 1
            while d < 8 do
                s <- s + "0"
                d <- d + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and floorf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: int = int x
        __ret <- float y
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length(s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
main()
