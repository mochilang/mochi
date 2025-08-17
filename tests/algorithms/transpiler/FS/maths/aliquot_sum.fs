// Generated 2025-08-17 08:49 +0700

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
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec aliquot_sum (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n <= 0 then
            ignore (failwith ("Input must be positive"))
        let mutable total: int = 0
        let mutable divisor: int = 1
        while divisor <= (_floordiv (int n) (int 2)) do
            if (((n % divisor + divisor) % divisor)) = 0 then
                total <- total + divisor
            divisor <- divisor + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (aliquot_sum (15))))
ignore (printfn "%s" (_str (aliquot_sum (6))))
ignore (printfn "%s" (_str (aliquot_sum (12))))
ignore (printfn "%s" (_str (aliquot_sum (1))))
ignore (printfn "%s" (_str (aliquot_sum (19))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
