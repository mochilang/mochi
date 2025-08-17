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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec remove_digit (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        let mutable n: int = num
        if n < 0 then
            n <- -n
        let mutable max_val: int = 0
        let mutable divisor: int = 1
        while divisor <= n do
            let higher: int = _floordiv (int n) (int (divisor * 10))
            let lower: int = ((n % divisor + divisor) % divisor)
            let candidate: int = (higher * divisor) + lower
            if candidate > max_val then
                max_val <- candidate
            divisor <- divisor * 10
        __ret <- max_val
        raise Return
        __ret
    with
        | Return -> __ret
and test_remove_digit () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (remove_digit (152)) <> 52 then
            ignore (failwith ("remove_digit(152) failed"))
        if (remove_digit (6385)) <> 685 then
            ignore (failwith ("remove_digit(6385) failed"))
        if (remove_digit (-11)) <> 1 then
            ignore (failwith ("remove_digit(-11) failed"))
        if (remove_digit (2222222)) <> 222222 then
            ignore (failwith ("remove_digit(2222222) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_remove_digit())
        ignore (printfn "%d" (remove_digit (152)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
