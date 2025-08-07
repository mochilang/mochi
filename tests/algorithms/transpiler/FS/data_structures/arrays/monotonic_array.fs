// Generated 2025-08-07 10:31 +0700

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
let rec is_monotonic (nums: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nums = nums
    try
        if (Seq.length (nums)) <= 2 then
            __ret <- true
            raise Return
        let mutable increasing: bool = true
        let mutable decreasing: bool = true
        let mutable i: int = 0
        while i < ((Seq.length (nums)) - 1) do
            if (_idx nums (i)) > (_idx nums (i + 1)) then
                increasing <- false
            if (_idx nums (i)) < (_idx nums (i + 1)) then
                decreasing <- false
            i <- i + 1
        __ret <- increasing || decreasing
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_monotonic (unbox<int array> [|1; 2; 2; 3|])))
printfn "%s" (_str (is_monotonic (unbox<int array> [|6; 5; 4; 4|])))
printfn "%s" (_str (is_monotonic (unbox<int array> [|1; 3; 2|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
