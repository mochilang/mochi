// Generated 2025-08-23 14:49 +0700

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
let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec largest_prime_factor (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        if n <= (int64 1) then
            __ret <- n
            raise Return
        let mutable i: int64 = int64 2
        let mutable ans: int64 = int64 0
        let mutable m: int64 = n
        if m = (int64 2) then
            __ret <- int64 2
            raise Return
        while m > (int64 2) do
            while (((m % i + i) % i)) <> (int64 0) do
                i <- i + (int64 1)
            ans <- i
            while (((m % i + i) % i)) = (int64 0) do
                m <- _floordiv64 (int64 m) (int64 i)
            i <- i + (int64 1)
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
ignore (json (largest_prime_factor (int64 13195)))
ignore (json (largest_prime_factor (int64 10)))
ignore (json (largest_prime_factor (int64 17)))
ignore (json (largest_prime_factor (600851475143L)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
