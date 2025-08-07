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
let rec equilibrium_index (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            total <- total + (_idx arr (i))
            i <- i + 1
        let mutable left: int = 0
        i <- 0
        while i < (Seq.length (arr)) do
            total <- total - (_idx arr (i))
            if left = total then
                __ret <- i
                raise Return
            left <- left + (_idx arr (i))
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let mutable arr1: int array = [|-7; 1; 5; 2; -4; 3; 0|]
printfn "%d" (equilibrium_index (arr1))
let mutable arr2: int array = [|1; 2; 3; 4; 5|]
printfn "%d" (equilibrium_index (arr2))
let mutable arr3: int array = [|1; 1; 1; 1; 1|]
printfn "%d" (equilibrium_index (arr3))
let mutable arr4: int array = [|2; 4; 6; 8; 10; 3|]
printfn "%d" (equilibrium_index (arr4))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
