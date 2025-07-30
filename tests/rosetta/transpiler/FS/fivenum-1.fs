// Generated 2025-07-30 21:05 +0700

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
let rec sortFloat (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable arr: float array = xs
        let mutable n: int = Seq.length arr
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let t: float = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- t
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec median (s: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    try
        let n: int = Seq.length s
        if (((n % 2 + 2) % 2)) = 1 then
            __ret <- s.[n / 2]
            raise Return
        __ret <- ((s.[(n / 2) - 1]) + (s.[n / 2])) / 2.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec fivenum (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable s: float array = sortFloat xs
        let n: int = Seq.length s
        let q1: float = s.[(n - 1) / 4]
        let med: float = median s
        let q3: float = s.[(3 * (n - 1)) / 4]
        __ret <- unbox<float array> [|s.[0]; q1; med; q3; s.[n - 1]|]
        raise Return
        __ret
    with
        | Return -> __ret
let x1: float array = [|36.0; 40.0; 7.0; 39.0; 41.0; 15.0|]
let x2: float array = [|15.0; 6.0; 42.0; 41.0; 7.0; 36.0; 49.0; 40.0; 39.0; 47.0; 43.0|]
let x3: float array = [|0.14082834; 0.0974879; 1.73131507; 0.87636009; -1.95059594; 0.73438555; -0.03035726; 1.4667597; -0.74621349; -0.72588772; 0.6390516; 0.61501527; -0.9898378; -1.00447874; -0.62759469; 0.66206163; 1.04312009; -0.10305385; 0.75775634; 0.32566578|]
printfn "%s" (string (fivenum x1))
printfn "%s" (string (fivenum x2))
printfn "%s" (string (fivenum x3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
