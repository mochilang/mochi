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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec tribonacci (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        let mutable dp: int array = [||]
        let mutable i: int = 0
        while i < num do
            if (i = 0) || (i = 1) then
                dp <- Array.append dp [|0|]
            else
                if i = 2 then
                    dp <- Array.append dp [|1|]
                else
                    let t: int = ((_idx dp (i - 1)) + (_idx dp (i - 2))) + (_idx dp (i - 3))
                    dp <- Array.append dp [|t|]
            i <- i + 1
        __ret <- dp
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (tribonacci (8)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
