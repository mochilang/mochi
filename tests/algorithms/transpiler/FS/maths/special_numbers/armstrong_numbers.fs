// Generated 2025-08-17 12:28 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and armstrong_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digits: int = 0
        let mutable temp: int = n
        while temp > 0 do
            temp <- _floordiv (int temp) (int 10)
            digits <- digits + 1
        let mutable total: int = 0
        temp <- n
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            total <- total + (pow_int (rem) (digits))
            temp <- _floordiv (int temp) (int 10)
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
and pluperfect_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digit_histogram: int array = Array.empty<int>
        let mutable i: int = 0
        while i < 10 do
            digit_histogram <- Array.append digit_histogram [|0|]
            i <- i + 1
        let mutable digit_total: int = 0
        let mutable temp: int = n
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            digit_histogram.[rem] <- (_idx digit_histogram (int rem)) + 1
            digit_total <- digit_total + 1
            temp <- _floordiv (int temp) (int 10)
        let mutable total: int = 0
        i <- 0
        while i < 10 do
            if (_idx digit_histogram (int i)) > 0 then
                total <- total + ((_idx digit_histogram (int i)) * (pow_int (i) (digit_total)))
            i <- i + 1
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
and narcissistic_number (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable digits: int = 0
        let mutable temp: int = n
        while temp > 0 do
            temp <- _floordiv (int temp) (int 10)
            digits <- digits + 1
        temp <- n
        let mutable total: int = 0
        while temp > 0 do
            let rem: int = ((temp % 10 + 10) % 10)
            total <- total + (pow_int (rem) (digits))
            temp <- _floordiv (int temp) (int 10)
        __ret <- total = n
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%b" (armstrong_number (371)))
ignore (printfn "%b" (armstrong_number (200)))
ignore (printfn "%b" (pluperfect_number (371)))
ignore (printfn "%b" (pluperfect_number (200)))
ignore (printfn "%b" (narcissistic_number (371)))
ignore (printfn "%b" (narcissistic_number (200)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
