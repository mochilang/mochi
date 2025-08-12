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
let rec is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n <= 1 then
            __ret <- false
            raise Return
        if n <= 3 then
            __ret <- true
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 3
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and is_germain_prime (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number < 1 then
            failwith ("Input value must be a positive integer")
        __ret <- (is_prime (number)) && (is_prime ((2 * number) + 1))
        raise Return
        __ret
    with
        | Return -> __ret
and is_safe_prime (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number < 1 then
            failwith ("Input value must be a positive integer")
        if ((((number - 1) % 2 + 2) % 2)) <> 0 then
            __ret <- false
            raise Return
        __ret <- (is_prime (number)) && (is_prime (_floordiv (number - 1) 2))
        raise Return
        __ret
    with
        | Return -> __ret
and test_is_germain_prime () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (is_germain_prime (3)) then
            failwith ("is_germain_prime(3) failed")
        if not (is_germain_prime (11)) then
            failwith ("is_germain_prime(11) failed")
        if is_germain_prime (4) then
            failwith ("is_germain_prime(4) failed")
        if not (is_germain_prime (23)) then
            failwith ("is_germain_prime(23) failed")
        if is_germain_prime (13) then
            failwith ("is_germain_prime(13) failed")
        if is_germain_prime (20) then
            failwith ("is_germain_prime(20) failed")
        __ret
    with
        | Return -> __ret
and test_is_safe_prime () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (is_safe_prime (5)) then
            failwith ("is_safe_prime(5) failed")
        if not (is_safe_prime (11)) then
            failwith ("is_safe_prime(11) failed")
        if is_safe_prime (1) then
            failwith ("is_safe_prime(1) failed")
        if is_safe_prime (2) then
            failwith ("is_safe_prime(2) failed")
        if is_safe_prime (3) then
            failwith ("is_safe_prime(3) failed")
        if not (is_safe_prime (47)) then
            failwith ("is_safe_prime(47) failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_is_germain_prime()
        test_is_safe_prime()
        printfn "%b" (is_germain_prime (23))
        printfn "%b" (is_safe_prime (47))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
