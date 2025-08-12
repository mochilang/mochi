// Generated 2025-08-12 13:41 +0700

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
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let temp: int = ((x % y + y) % y)
            x <- y
            y <- temp
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and solution (max_d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max_d = max_d
    try
        let mutable fractions_number: int = 0
        let mutable d: int = 0
        while d <= max_d do
            let mutable n: int = (_floordiv d 3) + 1
            let half: int = _floordiv (d + 1) 2
            while n < half do
                if (gcd (n) (d)) = 1 then
                    fractions_number <- fractions_number + 1
                n <- n + 1
            d <- d + 1
        __ret <- fractions_number
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%d" (solution (12000)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
