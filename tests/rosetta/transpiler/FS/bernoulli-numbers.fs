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
let rec bernoulli (n: int) =
    let mutable __ret : bignum = Unchecked.defaultof<bignum>
    let mutable n = n
    try
        let mutable a: bignum array = [||]
        let mutable m: int = 0
        while m <= n do
            a <- Array.append a [|(unbox<bignum> 1) / (unbox<bignum> (m + 1))|]
            let mutable j: int = m
            while j >= 1 do
                a.[j - 1] <- (unbox<bignum> j) * ((a.[j - 1]) - (a.[j]))
                j <- j - 1
            m <- m + 1
        __ret <- a.[0]
        raise Return
        __ret
    with
        | Return -> __ret
let rec padStart (s: string) (width: int) (pad: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    let mutable pad = pad
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- pad + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
for i in 0 .. (61 - 1) do
    let b: bignum = bernoulli (int i)
    if (int (num b)) <> 0 then
        let numStr: string = string (num b)
        let denStr: string = string (denom b)
        printfn "%s" ((((("B(" + (unbox<string> (padStart (string i) 2 " "))) + ") =") + (unbox<string> (padStart numStr 45 " "))) + "/") + denStr)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
