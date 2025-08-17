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
let rec combinations (n: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable k = k
    try
        if (k < 0) || (n < k) then
            ignore (failwith ("Please enter positive integers for n and k where n >= k"))
        let mutable res: int = 1
        let mutable i: int = 0
        while i < k do
            res <- res * (n - i)
            res <- _floordiv (int res) (int (i + 1))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("The number of five-card hands possible from a standard fifty-two card deck is: " + (_str (combinations (52) (5)))))
ignore (printfn "%s" (""))
ignore (printfn "%s" (("If a class of 40 students must be arranged into groups of 4 for group projects, there are " + (_str (combinations (40) (4)))) + " ways to arrange them."))
ignore (printfn "%s" (""))
ignore (printfn "%s" (("If 10 teams are competing in a Formula One race, there are " + (_str (combinations (10) (3)))) + " ways that first, second and third place can be awarded."))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
