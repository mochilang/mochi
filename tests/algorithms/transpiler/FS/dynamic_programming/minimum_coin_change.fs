// Generated 2025-08-07 15:46 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec dp_count (s: int array) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable n = n
    try
        if n < 0 then
            __ret <- 0
            raise Return
        let mutable table: int array = [||]
        let mutable i: int = 0
        while i <= n do
            table <- Array.append table [|0|]
            i <- i + 1
        table.[0] <- 1
        let mutable idx: int = 0
        while idx < (Seq.length (s)) do
            let coin_val: int = _idx s (idx)
            let mutable j: int = coin_val
            while j <= n do
                table.[j] <- (_idx table (j)) + (_idx table (j - coin_val))
                j <- j + 1
            idx <- idx + 1
        __ret <- _idx table (n)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (dp_count (unbox<int array> [|1; 2; 3|]) (4))
printfn "%d" (dp_count (unbox<int array> [|1; 2; 3|]) (7))
printfn "%d" (dp_count (unbox<int array> [|2; 5; 3; 6|]) (10))
printfn "%d" (dp_count (unbox<int array> [|10|]) (99))
printfn "%d" (dp_count (unbox<int array> [|4; 5; 6|]) (0))
printfn "%d" (dp_count (unbox<int array> [|1; 2; 3|]) (-5))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
