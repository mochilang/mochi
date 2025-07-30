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
open System

let rec randPerm (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < n do
            arr <- Array.append arr [|i|]
            i <- i + 1
        let mutable idx: int = n - 1
        while idx > 0 do
            let j = (((_now()) % (idx + 1) + (idx + 1)) % (idx + 1))
            let tmp: int = arr.[idx]
            arr.[idx] <- arr.[j]
            arr.[j] <- tmp
            idx <- idx - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and even (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable r: int array = [||]
        for x in xs do
            if (((x % 2 + 2) % 2)) = 0 then
                r <- Array.append r [|x|]
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and reduceToEven (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable last: int = 0
        let mutable i: int = 0
        while i < (Seq.length arr) do
            let e: int = arr.[i]
            if (((e % 2 + 2) % 2)) = 0 then
                arr.[last] <- e
                last <- last + 1
            i <- i + 1
        __ret <- Array.sub arr 0 (last - 0)
        raise Return
        __ret
    with
        | Return -> __ret
and listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (xs.[i]))
            if (i + 1) < (Seq.length xs) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable a: int array = randPerm 20
        let mutable cap_a: int = 20
        printfn "%s" (listStr a)
        printfn "%s" (listStr (even a))
        printfn "%s" (listStr a)
        a <- reduceToEven a
        printfn "%s" (listStr a)
        printfn "%s" ((("a len: " + (string (Seq.length a))) + " cap: ") + (string cap_a))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
