// Generated 2025-08-07 14:57 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec isPrime (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number < 2 then
            __ret <- false
            raise Return
        if number < 4 then
            __ret <- true
            raise Return
        if (((number % 2 + 2) % 2)) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 3
        while (i * i) <= number do
            if (((number % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec nextPrime (value: int) (factor: int) (desc: bool) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    let mutable factor = factor
    let mutable desc = desc
    try
        let mutable v: int = value * factor
        let firstValue: int = v
        while not (isPrime (v)) do
            if desc then
                v <- v - 1
            else
                v <- v + 1
        if v = firstValue then
            if desc then
                __ret <- nextPrime (v - 1) (1) (desc)
                raise Return
            else
                __ret <- nextPrime (v + 1) (1) (desc)
                raise Return
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (isPrime (0))
printfn "%b" (isPrime (1))
printfn "%b" (isPrime (2))
printfn "%b" (isPrime (3))
printfn "%b" (isPrime (27))
printfn "%b" (isPrime (87))
printfn "%b" (isPrime (563))
printfn "%b" (isPrime (2999))
printfn "%b" (isPrime (67483))
printfn "%d" (nextPrime (14) (1) (false))
printfn "%d" (nextPrime (14) (1) (true))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
