// Generated 2025-08-03 16:22 +0700

exception Return
let mutable __ret = ()

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let digits: string = "0123456789abcdef"
let rec toBase (n: int) (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable b = b
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable v: int = n
        let mutable out: string = ""
        while v > 0 do
            let d: int = ((v % b + b) % b)
            out <- (digits.Substring(d, (d + 1) - d)) + out
            v <- v / b
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and uabs (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then (a - b) else (b - a)
        raise Return
        __ret
    with
        | Return -> __ret
and isEsthetic (n: int) (b: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable b = b
    try
        if n = 0 then
            __ret <- false
            raise Return
        let mutable i: int = ((n % b + b) % b)
        n <- n / b
        while n > 0 do
            let j: int = ((n % b + b) % b)
            if (uabs (i) (j)) <> 1 then
                __ret <- false
                raise Return
            n <- n / b
            i <- j
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let mutable esths: int array = [||]
let rec dfs (n: int) (m: int) (i: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable m = m
    let mutable i = i
    try
        if (i >= n) && (i <= m) then
            esths <- Array.append esths [|i|]
        if (i = 0) || (i > m) then
            __ret <- ()
            raise Return
        let d: int = ((i % 10 + 10) % 10)
        let i1: int = ((i * 10) + d) - 1
        let i2: int = i1 + 2
        if d = 0 then
            dfs (n) (m) (i2)
        else
            if d = 9 then
                dfs (n) (m) (i1)
            else
                dfs (n) (m) (i1)
                dfs (n) (m) (i2)
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string (n)
        let mutable i: int = (String.length (s)) - 3
        while i >= 1 do
            s <- ((s.Substring(0, i - 0)) + ",") + (s.Substring(i, (String.length (s)) - i))
            i <- i - 3
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and listEsths (n: int) (n2: int) (m: int) (m2: int) (perLine: int) (showAll: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable n2 = n2
    let mutable m = m
    let mutable m2 = m2
    let mutable perLine = perLine
    let mutable showAll = showAll
    try
        esths <- Array.empty<int>
        let mutable i: int = 0
        while i < 10 do
            dfs (n2) (m2) (i)
            i <- i + 1
        let le: int = Seq.length (esths)
        printfn "%s" (((((("Base 10: " + (commatize (le))) + " esthetic numbers between ") + (commatize (n))) + " and ") + (commatize (m))) + ":")
        if showAll then
            let mutable c: int = 0
            let mutable line: string = ""
            for v in esths do
                if (String.length (line)) > 0 then
                    line <- line + " "
                line <- line + (string (v))
                c <- c + 1
                if (((c % perLine + perLine) % perLine)) = 0 then
                    printfn "%s" (line)
                    line <- ""
            if (String.length (line)) > 0 then
                printfn "%s" (line)
        else
            let mutable line: string = ""
            let mutable idx: int = 0
            while idx < perLine do
                if (String.length (line)) > 0 then
                    line <- line + " "
                line <- line + (string (_idx esths idx))
                idx <- idx + 1
            printfn "%s" (line)
            printfn "%s" ("............")
            line <- ""
            idx <- le - perLine
            while idx < le do
                if (String.length (line)) > 0 then
                    line <- line + " "
                line <- line + (string (_idx esths idx))
                idx <- idx + 1
            printfn "%s" (line)
        printfn "%s" ("")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable b: int = 2
        while b <= 16 do
            let start: int = 4 * b
            let stop: int = 6 * b
            printfn "%s" (((((("Base " + (string (b))) + ": ") + (string (start))) + "th to ") + (string (stop))) + "th esthetic numbers:")
            let mutable n: int = 1
            let mutable c: int = 0
            let mutable line: string = ""
            while c < stop do
                if isEsthetic (n) (b) then
                    c <- c + 1
                    if c >= start then
                        if (String.length (line)) > 0 then
                            line <- line + " "
                        line <- line + (toBase (n) (b))
                n <- n + 1
            printfn "%s" (line)
            printfn "%s" ("")
            b <- b + 1
        listEsths (1000) (1010) (9999) (9898) (16) (true)
        listEsths (100000000) (101010101) (130000000) (123456789) (9) (true)
        listEsths (int (int 100000000000L)) (int (int 101010101010L)) (int (int 130000000000L)) (int (int 123456789898L)) (7) (false)
        listEsths (int (int 100000000000000L)) (int (int 101010101010101L)) (int (int 130000000000000L)) (int (int 123456789898989L)) (5) (false)
        listEsths (int (int 100000000000000000L)) (int (int 101010101010101010L)) (int (int 130000000000000000L)) (int (int 123456789898989898L)) (4) (false)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
