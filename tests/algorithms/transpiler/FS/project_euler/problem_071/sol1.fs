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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec solution (numerator: int) (denominator: int) (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable numerator = numerator
    let mutable denominator = denominator
    let mutable limit = limit
    try
        let mutable maxNumerator: int = 0
        let mutable maxDenominator: int = 1
        let mutable currentDenominator: int = 1
        while currentDenominator <= limit do
            let mutable currentNumerator: int = _floordiv (currentDenominator * numerator) denominator
            if (((currentDenominator % denominator + denominator) % denominator)) = 0 then
                currentNumerator <- currentNumerator - 1
            if (currentNumerator * maxDenominator) > (currentDenominator * maxNumerator) then
                maxNumerator <- currentNumerator
                maxDenominator <- currentDenominator
            currentDenominator <- currentDenominator + 1
        __ret <- maxNumerator
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (3) (7) (1000000))))
ignore (printfn "%s" (_str (solution (3) (7) (8))))
ignore (printfn "%s" (_str (solution (6) (7) (60))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
