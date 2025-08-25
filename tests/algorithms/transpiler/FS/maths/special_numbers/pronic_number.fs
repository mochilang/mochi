// Generated 2025-08-25 22:27 +0700

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
let rec int_sqrt (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable r: int64 = int64 0
        while ((r + (int64 1)) * (r + (int64 1))) <= n do
            r <- r + (int64 1)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and is_pronic (n: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < (int64 0) then
            __ret <- false
            raise Return
        if (((n % (int64 2) + (int64 2)) % (int64 2))) <> (int64 0) then
            __ret <- false
            raise Return
        let root: int64 = int_sqrt (int64 n)
        __ret <- n = (root * (root + (int64 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and test_is_pronic () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if is_pronic (int64 (-1)) then
            ignore (failwith ("-1 should not be pronic"))
        if not (is_pronic (int64 0)) then
            ignore (failwith ("0 should be pronic"))
        if not (is_pronic (int64 2)) then
            ignore (failwith ("2 should be pronic"))
        if is_pronic (int64 5) then
            ignore (failwith ("5 should not be pronic"))
        if not (is_pronic (int64 6)) then
            ignore (failwith ("6 should be pronic"))
        if is_pronic (int64 8) then
            ignore (failwith ("8 should not be pronic"))
        if not (is_pronic (int64 30)) then
            ignore (failwith ("30 should be pronic"))
        if is_pronic (int64 32) then
            ignore (failwith ("32 should not be pronic"))
        if not (is_pronic (int64 2147441940)) then
            ignore (failwith ("2147441940 should be pronic"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_is_pronic())
        ignore (printfn "%b" (is_pronic (int64 56)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
