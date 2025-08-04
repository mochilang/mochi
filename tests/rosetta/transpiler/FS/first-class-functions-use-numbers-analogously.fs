// Generated 2025-08-05 01:50 +0700

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
let rec multiplier (n1: float) (n2: float) =
    let mutable __ret : float -> float = Unchecked.defaultof<float -> float>
    let mutable n1 = n1
    let mutable n2 = n2
    try
        let n1n2: float = n1 * n2
        __ret <- unbox<float -> float> (        fun (m: float) -> (n1n2 * m))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let x: float = 2.0
        let xi: float = 0.5
        let y: float = 4.0
        let yi: float = 0.25
        let z: float = x + y
        let zi: float = 1.0 / (x + y)
        let numbers: float array = [|x; y; z|]
        let inverses: float array = [|xi; yi; zi|]
        let mutable mfs: (float -> float) array = [||]
        let mutable i: int = 0
        while i < (Seq.length(numbers)) do
            mfs <- Array.append mfs [|multiplier (_idx numbers (i)) (_idx inverses (i))|]
            i <- i + 1
        for mf in mfs do
            printfn "%s" (string (mf (1.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
