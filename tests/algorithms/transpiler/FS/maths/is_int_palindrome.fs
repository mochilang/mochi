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
let rec is_int_palindrome (num: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num < 0 then
            __ret <- false
            raise Return
        let mutable n: int = num
        let mutable rev: int = 0
        while n > 0 do
            rev <- (rev * 10) + (((n % 10 + 10) % 10))
            n <- _floordiv n 10
        __ret <- rev = num
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%b" (is_int_palindrome (-121))
        printfn "%b" (is_int_palindrome (0))
        printfn "%b" (is_int_palindrome (10))
        printfn "%b" (is_int_palindrome (11))
        printfn "%b" (is_int_palindrome (101))
        printfn "%b" (is_int_palindrome (120))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
