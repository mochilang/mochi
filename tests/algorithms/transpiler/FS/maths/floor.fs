// Generated 2025-08-12 07:47 +0700

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
let rec floor (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let i: int = int (x)
        if (float (x - (float (float (i))))) >= 0.0 then
            __ret <- i
            raise Return
        __ret <- i - 1
        raise Return
        __ret
    with
        | Return -> __ret
and test_floor () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let nums: float array = unbox<float array> [|1.0; -1.0; 0.0; 0.0; 1.1; -1.1; 1.0; -1.0; 1000000000.0|]
        let expected: int array = unbox<int array> [|1; -1; 0; 0; 1; -2; 1; -1; 1000000000|]
        let mutable idx: int = 0
        while idx < (Seq.length (nums)) do
            if (floor (_idx nums (int idx))) <> (_idx expected (int idx)) then
                failwith ("floor test failed")
            idx <- idx + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_floor()
        printfn "%s" (_str (floor (-1.1)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
