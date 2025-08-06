// Generated 2025-08-06 21:33 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec bitwise_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable result: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let ax: int = ((x % 2 + 2) % 2)
            let by: int = ((y % 2 + 2) % 2)
            if ((((ax + by) % 2 + 2) % 2)) = 1 then
                result <- result + bit
            x <- x / 2
            y <- y / 2
            bit <- bit * 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and bitwise_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable result: int = 0
        let mutable bit: int = 1
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) && (y > 0) do
            if ((((x % 2 + 2) % 2)) = 1) && ((((y % 2 + 2) % 2)) = 1) then
                result <- result + bit
            x <- x / 2
            y <- y / 2
            bit <- bit * 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and bitwise_addition_recursive (number: int) (other_number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    let mutable other_number = other_number
    try
        if (number < 0) || (other_number < 0) then
            failwith ("Both arguments MUST be non-negative!")
        let bitwise_sum: int = bitwise_xor (number) (other_number)
        let carry: int = bitwise_and (number) (other_number)
        if carry = 0 then
            __ret <- bitwise_sum
            raise Return
        __ret <- bitwise_addition_recursive (bitwise_sum) (carry * 2)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (bitwise_addition_recursive (4) (5)))
printfn "%s" (_str (bitwise_addition_recursive (8) (9)))
printfn "%s" (_str (bitwise_addition_recursive (0) (4)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
