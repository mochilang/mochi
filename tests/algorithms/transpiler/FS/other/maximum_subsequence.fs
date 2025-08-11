// Generated 2025-08-09 23:14 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec max_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a >= b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
let rec max_subsequence_sum (nums: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("input sequence should not be empty")
        let mutable ans: int = _idx nums (int 0)
        let mutable i: int = 1
        while i < (Seq.length (nums)) do
            let num: int = _idx nums (int i)
            let extended: int = ans + num
            ans <- max_int (max_int (ans) (extended)) (num)
            i <- i + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (max_subsequence_sum (unbox<int array> [|1; 2; 3; 4; -2|]))
printfn "%d" (max_subsequence_sum (unbox<int array> [|-2; -3; -1; -4; -6|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
