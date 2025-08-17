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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec hexagonal_numbers (length: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable length = length
    try
        if length <= 0 then
            ignore (failwith ("Length must be a positive integer."))
        let mutable res: int array = Array.empty<int>
        let mutable n: int = 0
        while n < length do
            res <- Array.append res [|(n * ((2 * n) - 1))|]
            n <- n + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and test_hexagonal_numbers () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let expected5: int array = unbox<int array> [|0; 1; 6; 15; 28|]
        let result5: int array = hexagonal_numbers (5)
        if result5 <> expected5 then
            ignore (failwith ("hexagonal_numbers(5) failed"))
        let expected10: int array = unbox<int array> [|0; 1; 6; 15; 28; 45; 66; 91; 120; 153|]
        let result10: int array = hexagonal_numbers (10)
        if result10 <> expected10 then
            ignore (failwith ("hexagonal_numbers(10) failed"))
        __ret
    with
        | Return -> __ret
ignore (test_hexagonal_numbers())
ignore (printfn "%s" (_str (hexagonal_numbers (5))))
ignore (printfn "%s" (_str (hexagonal_numbers (10))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
