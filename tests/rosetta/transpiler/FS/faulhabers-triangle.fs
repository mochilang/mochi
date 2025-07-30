// Generated 2025-07-30 21:05 +0700

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

let _padStart (s:string) (width:int) (pad:string) =
    let mutable out = s
    while out.Length < width do
        out <- pad + out
    out

let rec bernoulli (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable a: float array = [||]
        let mutable m: int = 0
        while m <= n do
            a <- Array.append a [|(float 1) / (float (m + 1))|]
            let mutable j: int = m
            while j >= 1 do
                a.[j - 1] <- (float j) * ((a.[j - 1]) - (a.[j]))
                j <- j - 1
            m <- m + 1
        if n <> 1 then
            __ret <- a.[0]
            raise Return
        __ret <- -(a.[0])
        raise Return
        __ret
    with
        | Return -> __ret
and binom (n: int) (k: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    let mutable k = k
    try
        if (k < 0) || (k > n) then
            __ret <- bigint 0
            raise Return
        let mutable kk: int = k
        if kk > (n - kk) then
            kk <- n - kk
        let mutable res: bigint = bigint 1
        let mutable i: int = 0
        while i < kk do
            res <- res * (bigint (n - i))
            i <- i + 1
            res <- res / (bigint i)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and faulhaberRow (p: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable p = p
    try
        let mutable coeffs: float array = [||]
        let mutable i: int = 0
        while i <= p do
            coeffs <- Array.append coeffs [|float 0|]
            i <- i + 1
        let mutable j: int = 0
        let mutable sign: int = -1
        while j <= p do
            sign <- -sign
            let mutable c: float = (float 1) / (float (p + 1))
            if sign < 0 then
                c <- -c
            c <- c * (float (binom (p + 1) j))
            c <- c * (float (bernoulli j))
            coeffs.[p - j] <- c
            j <- j + 1
        __ret <- coeffs
        raise Return
        __ret
    with
        | Return -> __ret
and ratStr (r: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    try
        let s: string = string r
        if endsWith s "/1" then
            __ret <- _substring s 0 ((String.length s) - 2)
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and endsWith (s: string) (suf: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable suf = suf
    try
        __ret <- if (String.length s) < (String.length suf) then false else ((_substring s ((String.length s) - (String.length suf)) (String.length s)) = suf)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable p: int = 0
        while p < 10 do
            let row: float array = faulhaberRow p
            let mutable line: string = ""
            let mutable idx: int = 0
            while idx < (Seq.length row) do
                line <- line + (unbox<string> (_padStart (ratStr (row.[idx])) 5 " "))
                if idx < ((Seq.length row) - 1) then
                    line <- line + "  "
                idx <- idx + 1
            printfn "%s" line
            p <- p + 1
        printfn "%s" ""
        let k: int = 17
        let coeffs: float array = faulhaberRow k
        let mutable nn: float = float 1000
        let mutable np: float = float 1
        let mutable sum: float = float 0
        let mutable i: int = 0
        while i < (Seq.length coeffs) do
            np <- np * nn
            sum <- sum + ((coeffs.[i]) * np)
            i <- i + 1
        printfn "%s" (ratStr sum)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
