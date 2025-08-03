// Generated 2025-08-03 23:01 +0700

exception Return
let mutable __ret = ()

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

let rec eulerSum () =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    try
        let mutable pow5: int array = [||]
        let mutable i: int = 0
        while i < 250 do
            pow5 <- Array.append pow5 [|(((i * i) * i) * i) * i|]
            i <- i + 1
        let mutable sums: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
        let mutable x2: int = 2
        while x2 < 250 do
            let mutable x3: int = 1
            while x3 < x2 do
                let s: int = (_idx pow5 x2) + (_idx pow5 x3)
                if not (sums.ContainsKey(s)) then
                    sums.[s] <- [|x2; x3|]
                x3 <- x3 + 1
            x2 <- x2 + 1
        let mutable x0: int = 4
        while x0 < 250 do
            let mutable x1: int = 3
            while x1 < x0 do
                let mutable y: int = x0 + 1
                while y < 250 do
                    let rem: int = ((_idx pow5 y) - (_idx pow5 x0)) - (_idx pow5 x1)
                    if sums.ContainsKey(rem) then
                        let pair: int array = sums.[rem]
                        let a: int = _idx pair 0
                        let b: int = _idx pair 1
                        if (x1 > a) && (a > b) then
                            __ret <- unbox<int array> [|x0; x1; a; b; y|]
                            raise Return
                    y <- y + 1
                x1 <- x1 + 1
            x0 <- x0 + 1
        __ret <- unbox<int array> [|0; 0; 0; 0; 0|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let r: int array = eulerSum()
        printfn "%s" (((((((((string (_idx r 0)) + " ") + (string (_idx r 1))) + " ") + (string (_idx r 2))) + " ") + (string (_idx r 3))) + " ") + (string (_idx r 4)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
