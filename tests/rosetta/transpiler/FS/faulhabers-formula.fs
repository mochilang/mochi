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
        __ret <- a.[0]
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
and coeff (p: int) (j: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable p = p
    let mutable j = j
    try
        let ``base``: float = (float 1) / (float (p + 1))
        let mutable c: float = ``base``
        if (((j % 2 + 2) % 2)) = 1 then
            c <- -c
        c <- c * (float (binom (p + 1) j))
        c <- c * (float (bernoulli j))
        __ret <- c
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
            let mutable line: string = (string p) + " :"
            let mutable j: int = 0
            while j <= p do
                let c: float = coeff p j
                if (string c) <> "0/1" then
                    line <- ((line + " ") + (string c)) + "×n"
                    let exp: int = (p + 1) - j
                    if exp > 1 then
                        line <- (line + "^") + (string exp)
                j <- j + 1
            printfn "%s" line
            p <- p + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
