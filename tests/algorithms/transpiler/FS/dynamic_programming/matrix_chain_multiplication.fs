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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let INF: int = 1000000000
let rec matrix_chain_multiply (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        if (Seq.length (arr)) < 2 then
            __ret <- 0
            raise Return
        let n: int = Seq.length (arr)
        let mutable dp: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|INF|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        i <- n - 1
        while i > 0 do
            let mutable j: int = i
            while j < n do
                if i = j then
                    dp.[i].[j] <- 0
                else
                    let mutable k: int = i
                    while k < j do
                        let cost: int = ((_idx (_idx dp (i)) (k)) + (_idx (_idx dp (k + 1)) (j))) + (((_idx arr (i - 1)) * (_idx arr (k))) * (_idx arr (j)))
                        if cost < (_idx (_idx dp (i)) (j)) then
                            dp.[i].[j] <- cost
                        k <- k + 1
                j <- j + 1
            i <- i - 1
        __ret <- _idx (_idx dp (1)) (n - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
