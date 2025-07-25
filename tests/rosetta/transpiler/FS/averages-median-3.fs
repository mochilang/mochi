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
open System

let rec qsel (a: float array) (k: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable k = k
    try
        let mutable arr = a
        while (int (Array.length arr)) > 1 do
            let mutable px: int = (((_now()) % (Array.length arr) + (Array.length arr)) % (Array.length arr))
            let mutable pv = arr.[px]
            let last: int = (int (Array.length arr)) - 1
            let tmp = arr.[px]
            arr.[px] <- arr.[last]
            arr.[last] <- tmp
            px <- 0
            let mutable i: int = 0
            while i < last do
                let v = arr.[i]
                if v < pv then
                    let tmp2 = arr.[px]
                    arr.[px] <- arr.[i]
                    arr.[i] <- tmp2
                    px <- px + 1
                i <- i + 1
            if px = k then
                __ret <- pv
                raise Return
            if k < px then
                arr <- Array.sub arr 0 (px - 0)
            else
                let tmp2 = arr.[px]
                arr.[px] <- pv
                arr.[last] <- tmp2
                arr <- Array.sub arr (px + 1) ((int (Array.length arr)) - (px + 1))
                k <- k - (px + 1)
        __ret <- arr.[0]
        raise Return
        __ret
    with
        | Return -> __ret
let rec median (list: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable list = list
    try
        let mutable arr = list
        let half: int = int ((int (Array.length arr)) / 2)
        let med: float = qsel arr half
        if (int ((((Array.length arr) % 2 + 2) % 2))) = 0 then
            __ret <- (float (med + (float (qsel arr (half - 1))))) / 2.0
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
