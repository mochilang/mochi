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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec find_missing_number (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        let low: int = int (Seq.min (nums))
        let high: int = int (Seq.max (nums))
        let count: int = (high - low) + 1
        let expected_sum: int = ((low + high) * count) / 2
        let mutable actual_sum: int = 0
        let mutable i: int = 0
        let n: int = Seq.length (nums)
        while i < n do
            actual_sum <- actual_sum + (_idx nums (i))
            i <- i + 1
        __ret <- expected_sum - actual_sum
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (find_missing_number (unbox<int array> [|0; 1; 3; 4|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
