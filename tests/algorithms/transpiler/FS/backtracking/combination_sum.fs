// Generated 2025-08-06 16:21 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec backtrack (candidates: int array) (start: int) (target: int) (path: int array) (result: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable candidates = candidates
    let mutable start = start
    let mutable target = target
    let mutable path = path
    let mutable result = result
    try
        if target = 0 then
            __ret <- Array.append result [|path|]
            raise Return
        let mutable i: int = start
        while i < (Seq.length(candidates)) do
            let value: int = _idx candidates (i)
            if value <= target then
                let new_path: int array = Array.append path [|value|]
                result <- backtrack (candidates) (i) (target - value) (new_path) (result)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and combination_sum (candidates: int array) (target: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable candidates = candidates
    let mutable target = target
    try
        let path: int array = [||]
        let mutable result: int array array = [||]
        __ret <- backtrack (candidates) (0) (target) (path) (result)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (combination_sum (unbox<int array> [|2; 3; 5|]) (8)))
printfn "%s" (_str (combination_sum (unbox<int array> [|2; 3; 6; 7|]) (7)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
