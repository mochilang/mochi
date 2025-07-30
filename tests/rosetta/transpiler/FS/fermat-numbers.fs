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

let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and pow_big (``base``: bigint) (exp: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: bigint = bigint 1
        let mutable b: bigint = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and parseBigInt (str: string) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((_substring str 0 1) = "-") then
            neg <- true
            i <- 1
        let mutable n: bigint = bigint 0
        while i < (String.length str) do
            let ch: string = _substring str i (i + 1)
            let d: int = int ch
            n <- (n * (bigint 10)) + (bigint d)
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and fermat (n: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable n = n
    try
        let p: int = pow_int 2 n
        __ret <- (pow_big (bigint 2) p) + (bigint 1)
        raise Return
        __ret
    with
        | Return -> __ret
and primeFactorsBig (n: bigint) =
    let mutable __ret : bigint array = Unchecked.defaultof<bigint array>
    let mutable n = n
    try
        let mutable factors: bigint array = [||]
        let mutable m: bigint = n
        let mutable d: bigint = bigint 2
        while (((m % d + d) % d)) = (bigint 0) do
            factors <- Array.append factors [|d|]
            m <- m / d
        d <- bigint 3
        while (d * d) <= m do
            while (((m % d + d) % d)) = (bigint 0) do
                factors <- Array.append factors [|d|]
                m <- m / d
            d <- d + (bigint 2)
        if m > (bigint 1) then
            factors <- Array.append factors [|m|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
and show_list (xs: bigint array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable line: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            line <- line + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                line <- line + " "
            i <- i + 1
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable nums: bigint array = [||]
        for i in 0 .. (8 - 1) do
            nums <- Array.append nums [|unbox<bigint> (fermat i)|]
        printfn "%s" "First 8 Fermat numbers:"
        for n in nums do
            printfn "%s" (string n)
        let extra: Map<int, bigint array> = Map.ofList [(6, [|bigint 274177; bigint 67280421310721L|]); (7, [|parseBigInt "59649589127497217"; parseBigInt "5704689200685129054721"|])]
        printfn "%s" "\nFactors:"
        let mutable i: int = 0
        while i < (Seq.length nums) do
            let mutable facs: bigint array = [||]
            if i <= 5 then
                facs <- primeFactorsBig (nums.[i])
            else
                facs <- unbox<bigint array> (extra.[i] |> unbox<bigint array>)
            printfn "%s" ((("F" + (string i)) + " = ") + (unbox<string> (show_list facs)))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
