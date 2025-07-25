// Generated 2025-07-26 05:05 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec applyFilter (input: float array) (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable input = input
    let mutable a = a
    let mutable b = b
    try
        let mutable out: float array = [||]
        let scale: float = 1.0 / (float (a.[0]))
        let mutable i: int = 0
        while i < (int (Array.length input)) do
            let mutable tmp: float = 0.0
            let mutable j: int = 0
            while (j <= i) && (j < (int (Array.length b))) do
                tmp <- tmp + (float ((b.[j]) * (input.[i - j])))
                j <- j + 1
            j <- 0
            while (j < i) && ((j + 1) < (int (Array.length a))) do
                tmp <- tmp - (float ((a.[j + 1]) * (out.[(i - j) - 1])))
                j <- j + 1
            out <- Array.append out [|tmp * scale|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let a: float array = [|1.0; -0.00000000000000027756; 0.33333333; -0.0000000000000000185|]
let b: float array = [|0.16666667; 0.5; 0.5; 0.16666667|]
let ``sig``: float array = [|-0.917843918645; 0.141984778794; 1.20536903482; 0.190286794412; -0.662370894973; -1.00700480494; -0.404707073677; 0.800482325044; 0.743500089861; 1.01090520172; 0.741527555207; 0.277841675195; 0.400833448236; -0.2085993586; -0.172842103641; -0.134316096293; 0.0259303398477; 0.490105989562; 0.549391221511; 0.9047198589|]
let res: float array = applyFilter ``sig`` a b
let mutable k: int = 0
while k < (int (Array.length res)) do
    printfn "%A" (res.[k])
    k <- k + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
