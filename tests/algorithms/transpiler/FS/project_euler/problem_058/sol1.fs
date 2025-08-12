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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec is_prime (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if (1 < number) && (number < 4) then
            __ret <- true
            raise Return
        if ((number < 2) || ((((number % 2 + 2) % 2)) = 0)) || ((((number % 3 + 3) % 3)) = 0) then
            __ret <- false
            raise Return
        let mutable i: int = 5
        while (i * i) <= number do
            if ((((number % i + i) % i)) = 0) || ((((number % (i + 2) + (i + 2)) % (i + 2))) = 0) then
                __ret <- false
                raise Return
            i <- i + 6
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution (ratio: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ratio = ratio
    try
        let mutable j: int = 3
        let mutable primes: int = 3
        while ((float primes) / (float ((2 * j) - 1))) >= ratio do
            let mutable i: int = ((j * j) + j) + 1
            let limit: int = (j + 2) * (j + 2)
            let step: int = j + 1
            while i < limit do
                if is_prime (i) then
                    primes <- primes + 1
                i <- i + step
            j <- j + 2
        __ret <- j
        raise Return
        __ret
    with
        | Return -> __ret
and test_solution () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (solution (0.5)) <> 11 then
            ignore (failwith ("solution 0.5 failed"))
        if (solution (0.2)) <> 309 then
            ignore (failwith ("solution 0.2 failed"))
        if (solution (0.111)) <> 11317 then
            ignore (failwith ("solution 0.111 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_solution()
        ignore (printfn "%s" (_str (solution (0.1))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
