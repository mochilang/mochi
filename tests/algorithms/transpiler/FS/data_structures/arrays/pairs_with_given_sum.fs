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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pairs_with_sum (arr: int array) (req_sum: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable req_sum = req_sum
    try
        let n: int = Seq.length (arr)
        let mutable count: int = 0
        let mutable i: int = 0
        while i < n do
            let mutable j: int = i + 1
            while j < n do
                if ((_idx arr (i)) + (_idx arr (j))) = req_sum then
                    count <- count + 1
                j <- j + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (pairs_with_sum (unbox<int array> [|1; 5; 7; 1|]) (6))
printfn "%d" (pairs_with_sum (unbox<int array> [|1; 1; 1; 1; 1; 1; 1; 1|]) (2))
printfn "%d" (pairs_with_sum (unbox<int array> [|1; 7; 6; 2; 5; 4; 3; 1; 9; 8|]) (7))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
