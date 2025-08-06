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
let rec pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and is_bit_set (number: int) (position: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    let mutable position = position
    try
        let shifted: int = number / (pow2 (position))
        let remainder: int = ((shifted % 2 + 2) % 2)
        __ret <- remainder = 1
        raise Return
        __ret
    with
        | Return -> __ret
and set_bit (number: int) (position: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    let mutable position = position
    try
        __ret <- if is_bit_set (number) (position) then number else (number + (pow2 (position)))
        raise Return
        __ret
    with
        | Return -> __ret
and clear_bit (number: int) (position: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    let mutable position = position
    try
        __ret <- if is_bit_set (number) (position) then (number - (pow2 (position))) else number
        raise Return
        __ret
    with
        | Return -> __ret
and flip_bit (number: int) (position: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    let mutable position = position
    try
        __ret <- if is_bit_set (number) (position) then (number - (pow2 (position))) else (number + (pow2 (position)))
        raise Return
        __ret
    with
        | Return -> __ret
and get_bit (number: int) (position: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    let mutable position = position
    try
        __ret <- if is_bit_set (number) (position) then 1 else 0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (set_bit (13) (1)))
printfn "%s" (_str (clear_bit (18) (1)))
printfn "%s" (_str (flip_bit (5) (1)))
printfn "%s" (_str (is_bit_set (10) (3)))
printfn "%s" (_str (get_bit (10) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
