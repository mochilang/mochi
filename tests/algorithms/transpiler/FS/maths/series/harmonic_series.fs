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
let rec harmonic_series (n_term: float) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable n_term = n_term
    try
        if n_term <= 0.0 then
            __ret <- Array.empty<string>
            raise Return
        let limit: int = int (n_term)
        let mutable series: string array = Array.empty<string>
        let mutable i: int = 0
        while i < limit do
            if i = 0 then
                series <- Array.append series [|"1"|]
            else
                series <- Array.append series [|("1/" + (_str (i + 1)))|]
            i <- i + 1
        __ret <- series
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (harmonic_series (5.0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
