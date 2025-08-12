// Generated 2025-08-12 08:17 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec binary_exponentiation (a: int) (n: int) (``mod``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    let mutable ``mod`` = ``mod``
    try
        if n = 0 then
            __ret <- 1
            raise Return
        if (((n % 2 + 2) % 2)) = 1 then
            __ret <- ((((binary_exponentiation (a) (n - 1) (``mod``)) * a) % ``mod`` + ``mod``) % ``mod``)
            raise Return
        let b: int = binary_exponentiation (a) (_floordiv n 2) (``mod``)
        __ret <- (((b * b) % ``mod`` + ``mod``) % ``mod``)
        raise Return
        __ret
    with
        | Return -> __ret
and naive_exponent_mod (a: int) (n: int) (``mod``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable n = n
    let mutable ``mod`` = ``mod``
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < n do
            result <- (((result * a) % ``mod`` + ``mod``) % ``mod``)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and print_bool (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        if b then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        __ret
    with
        | Return -> __ret
let p: int = 701
let a: int = 1000000000
let b: int = 10
let left: int = (((_floordiv a b) % p + p) % p)
let right_fast: int = (((a * (binary_exponentiation (b) (p - 2) (p))) % p + p) % p)
print_bool (left = right_fast)
let right_naive: int = (((a * (naive_exponent_mod (b) (p - 2) (p))) % p + p) % p)
print_bool (left = right_naive)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
