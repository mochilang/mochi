// Generated 2025-07-26 04:38 +0700

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
let rec sel (list: float array) (k: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable list = list
    let mutable k = k
    try
        let mutable i: int = 0
        while i <= k do
            let mutable minIndex: int = i
            let mutable j: int = i + 1
            while j < (int (Array.length list)) do
                if (list.[j]) < (list.[minIndex]) then
                    minIndex <- j
                j <- j + 1
            let tmp: float = list.[i]
            list.[i] <- list.[minIndex]
            list.[minIndex] <- tmp
            i <- i + 1
        __ret <- list.[k]
        raise Return
        __ret
    with
        | Return -> __ret
let rec median (a: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    try
        let mutable arr = a
        let half: int = int ((int (Array.length arr)) / 2)
        let med: float = sel arr half
        if (int ((((Array.length arr) % 2 + 2) % 2))) = 0 then
            __ret <- (float (med + (float (arr.[half - 1])))) / 2.0
            raise Return
        __ret <- med
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (string (median [|3.0; 1.0; 4.0; 1.0|]))
printfn "%s" (string (median [|3.0; 1.0; 4.0; 1.0; 5.0|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
