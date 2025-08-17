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
let rec decimal_to_negative_base_2 (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        if num = 0 then
            __ret <- 0
            raise Return
        let mutable n: int = num
        let mutable ans: string = ""
        while n <> 0 do
            let mutable rem: int = ((n % (-2) + (-2)) % (-2))
            n <- _floordiv (int n) (int (-2))
            if rem < 0 then
                rem <- rem + 2
                n <- n + 1
            ans <- (_str (rem)) + ans
        __ret <- int (ans)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%d" (decimal_to_negative_base_2 (0)))
ignore (printfn "%d" (decimal_to_negative_base_2 (-19)))
ignore (printfn "%d" (decimal_to_negative_base_2 (4)))
ignore (printfn "%d" (decimal_to_negative_base_2 (7)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
