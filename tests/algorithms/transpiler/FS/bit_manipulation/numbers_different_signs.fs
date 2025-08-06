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
let rec different_signs (num1: int) (num2: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num1 = num1
    let mutable num2 = num2
    try
        let sign1: bool = num1 < 0
        let sign2: bool = num2 < 0
        __ret <- sign1 <> sign2
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (different_signs (1) (-1)))
printfn "%s" (_str (different_signs (1) (1)))
printfn "%s" (_str (different_signs (int 1000000000000000000L) (int (-1000000000000000000L))))
printfn "%s" (_str (different_signs (int (-1000000000000000000L)) (int 1000000000000000000L)))
printfn "%s" (_str (different_signs (50) (278)))
printfn "%s" (_str (different_signs (0) (2)))
printfn "%s" (_str (different_signs (2) (0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
