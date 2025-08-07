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
let rec product_sum (arr: obj array) (depth: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable depth = depth
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let el: obj = _idx arr (i)
            if not (Seq.isEmpty (el)) then
                total <- total + (product_sum (unbox<obj array> el) (depth + 1))
            else
                total <- total + (unbox<int> el)
            i <- i + 1
        __ret <- total * depth
        raise Return
        __ret
    with
        | Return -> __ret
let rec product_sum_array (array: obj array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    try
        let res: int = product_sum (array) (1)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let example: obj array = [|box (5); box (2); box ([|-7; 1|]); box (3); box ([|box (6); box ([|-13; 8|]); box (4)|])|]
printfn "%d" (product_sum_array (example))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
