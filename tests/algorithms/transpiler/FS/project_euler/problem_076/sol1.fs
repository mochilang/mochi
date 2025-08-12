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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec solution (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    try
        let mutable memo: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i <= m do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < m do
                row <- Array.append row [|0|]
                j <- j + 1
            memo <- Array.append memo [|row|]
            i <- i + 1
        i <- 0
        while i <= m do
            memo.[i].[0] <- 1
            i <- i + 1
        let mutable n: int = 0
        while n <= m do
            let mutable k: int = 1
            while k < m do
                memo.[n].[k] <- (_idx (_idx memo (int n)) (int k)) + (_idx (_idx memo (int n)) (int (k - 1)))
                if n > k then
                    memo.[n].[k] <- (_idx (_idx memo (int n)) (int k)) + (_idx (_idx memo (int ((n - k) - 1))) (int k))
                k <- k + 1
            n <- n + 1
        __ret <- (_idx (_idx memo (int m)) (int (m - 1))) - 1
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (100))))
ignore (printfn "%s" (_str (solution (50))))
ignore (printfn "%s" (_str (solution (30))))
ignore (printfn "%s" (_str (solution (10))))
ignore (printfn "%s" (_str (solution (5))))
ignore (printfn "%s" (_str (solution (3))))
ignore (printfn "%s" (_str (solution (2))))
ignore (printfn "%s" (_str (solution (1))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
