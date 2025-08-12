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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sum_digits (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        let mutable n: int = num
        let mutable digit_sum: int = 0
        while n > 0 do
            digit_sum <- digit_sum + (((n % 10 + 10) % 10))
            n <- _floordiv n 10
        __ret <- digit_sum
        raise Return
        __ret
    with
        | Return -> __ret
and solution (max_n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max_n = max_n
    try
        let mutable pre_numerator: int = 1
        let mutable cur_numerator: int = 2
        let mutable i: int = 2
        while i <= max_n do
            let temp: int = pre_numerator
            let mutable e_cont: int = 1
            if (((i % 3 + 3) % 3)) = 0 then
                e_cont <- _floordiv (2 * i) 3
            pre_numerator <- cur_numerator
            cur_numerator <- (e_cont * pre_numerator) + temp
            i <- i + 1
        __ret <- sum_digits (cur_numerator)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%d" (solution (9)))
ignore (printfn "%d" (solution (10)))
ignore (printfn "%d" (solution (50)))
ignore (printfn "%d" (solution (100)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
