// Generated 2025-08-23 15:31 +0700

exception Break
exception Continue

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
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec count_divisors (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable m: int64 = n
        let mutable n_divisors: int64 = 1L
        let mutable i: int64 = 2L
        while (i * i) <= m do
            let mutable multiplicity: int64 = 0L
            while (((m % i + i) % i)) = 0L do
                m <- _floordiv64 (int64 m) (int64 i)
                multiplicity <- multiplicity + 1L
            n_divisors <- n_divisors * (multiplicity + 1L)
            i <- i + 1L
        if m > 1L then
            n_divisors <- n_divisors * 2L
        __ret <- n_divisors
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    try
        let mutable t_num: int64 = 1L
        let mutable i: int64 = 1L
        try
            while true do
                try
                    i <- i + 1L
                    t_num <- t_num + i
                    if (count_divisors (t_num)) > 500L then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- t_num
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution())))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
