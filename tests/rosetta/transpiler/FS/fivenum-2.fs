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
open System

let rec qsel (a: float array) (k: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable k = k
    try
        let mutable arr: float array = a
        while (Seq.length arr) > 1 do
            let mutable px: int = (((_now()) % (Seq.length arr) + (Seq.length arr)) % (Seq.length arr))
            let mutable pv: float = arr.[px]
            let last: int = (Seq.length arr) - 1
            let tmp: float = arr.[px]
            arr.[px] <- arr.[last]
            arr.[last] <- tmp
            px <- 0
            let mutable i: int = 0
            while i < last do
                let v: float = arr.[i]
                if v < pv then
                    let t: float = arr.[px]
                    arr.[px] <- arr.[i]
                    arr.[i] <- t
                    px <- px + 1
                i <- i + 1
            arr.[px] <- pv
            if px = k then
                __ret <- pv
                raise Return
            if k < px then
                arr <- Array.sub arr 0 (px - 0)
            else
                arr <- Array.sub arr (px + 1) ((int (Array.length arr)) - (px + 1))
                k <- k - (px + 1)
        __ret <- arr.[0]
        raise Return
        __ret
    with
        | Return -> __ret
let rec fivenum (a: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    try
        let last: int = (Seq.length a) - 1
        let m: int = last / 2
        let mutable n5: float array = [||]
        n5 <- Array.append n5 [|unbox<float> (qsel (Array.sub a 0 (m - 0)) 0)|]
        n5 <- Array.append n5 [|unbox<float> (qsel (Array.sub a 0 (m - 0)) ((Seq.length a) / 4))|]
        n5 <- Array.append n5 [|unbox<float> (qsel a m)|]
        let mutable arr2: float array = Array.sub a m ((int (Array.length a)) - m)
        let q3: int = (last - m) - ((Seq.length a) / 4)
        n5 <- Array.append n5 [|unbox<float> (qsel arr2 q3)|]
        arr2 <- Array.sub arr2 q3 ((int (Array.length arr2)) - q3)
        n5 <- Array.append n5 [|unbox<float> (qsel arr2 ((Seq.length arr2) - 1))|]
        __ret <- n5
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
