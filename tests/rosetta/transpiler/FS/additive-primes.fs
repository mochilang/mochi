// Generated 2025-07-26 19:29 +0700

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
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and sumDigits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable s: int = 0
        let mutable x: int = n
        while x > 0 do
            s <- s + (((x % 10 + 10) % 10))
            x <- int (x / 10)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n < 10 then
            __ret <- "  " + (string n)
            raise Return
        if n < 100 then
            __ret <- " " + (string n)
            raise Return
        __ret <- string n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "Additive primes less than 500:"
        let mutable count: int = 0
        let mutable line: string = ""
        let mutable lineCount: int = 0
        let mutable i: int = 2
        while i < 500 do
            if (isPrime i) && (isPrime (sumDigits i)) then
                count <- count + 1
                line <- (line + (unbox<string> (pad i))) + "  "
                lineCount <- lineCount + 1
                if lineCount = 10 then
                    printfn "%s" (_substring line 0 ((String.length line) - 2))
                    line <- ""
                    lineCount <- 0
            if i > 2 then
                i <- i + 2
            else
                i <- i + 1
        if lineCount > 0 then
            printfn "%s" (_substring line 0 ((String.length line) - 2))
        printfn "%s" ((string count) + " additive primes found.")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
