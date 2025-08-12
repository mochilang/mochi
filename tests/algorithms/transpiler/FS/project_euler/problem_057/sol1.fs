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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec solution (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable prev_num: int = 1
        let mutable prev_den: int = 1
        let mutable count: int = 0
        let mutable i: int = 1
        while i <= n do
            let num: int = prev_num + (2 * prev_den)
            let den: int = prev_num + prev_den
            if (String.length (_str (num))) > (String.length (_str (den))) then
                count <- count + 1
            prev_num <- num
            prev_den <- den
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%d" (solution (14)))
ignore (printfn "%d" (solution (100)))
ignore (printfn "%d" (solution (1000)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
