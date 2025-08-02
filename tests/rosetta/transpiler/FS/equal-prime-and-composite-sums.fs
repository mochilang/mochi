// Generated 2025-08-02 20:24 +0700

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
let _padStart (s:string) (width:int) (pad:string) =
    let mutable out = s
    while out.Length < width do
        out <- pad + out
    out

let rec commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string (n)
        let mutable i: int = (String.length (s)) - 3
        while i >= 1 do
            s <- ((s.Substring(0, i - 0)) + ",") + (s.Substring(i, (String.length (s)) - i))
            i <- i - 3
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and primeSieve (n: int) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    try
        let mutable sieve: bool array = [||]
        let mutable i: int = 0
        while i <= n do
            sieve <- Array.append sieve [|false|]
            i <- i + 1
        sieve.[0] <- true
        sieve.[1] <- true
        let mutable p: int = 2
        while (p * p) <= n do
            if not (sieve.[p]) then
                let mutable m: int = p * p
                while m <= n do
                    sieve.[m] <- true
                    m <- m + p
            p <- p + 1
        __ret <- sieve
        raise Return
        __ret
    with
        | Return -> __ret
and search (xs: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable target = target
    try
        let mutable low: int = 0
        let mutable high: int = Seq.length (xs)
        while low < high do
            let mutable mid: int = (low + high) / 2
            if (xs.[mid]) < target then
                low <- mid + 1
            else
                high <- mid
        __ret <- low
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let limit: int = 45000
        let compMap: bool array = primeSieve (limit)
        let mutable compSums: int array = [||]
        let mutable primeSums: int array = [||]
        let mutable csum: int = 0
        let mutable psum: int = 0
        let mutable i: int = 2
        while i <= limit do
            if compMap.[i] then
                csum <- csum + i
                compSums <- Array.append compSums [|csum|]
            else
                psum <- psum + i
                primeSums <- Array.append primeSums [|psum|]
            i <- i + 1
        printfn "%s" ("Sum        | Prime Index | Composite Index")
        printfn "%s" ("------------------------------------------")
        let mutable idx: int = 0
        while idx < (Seq.length (primeSums)) do
            let mutable s: int = primeSums.[idx]
            let j: int = search (compSums) (s)
            if (j < (Seq.length (compSums))) && ((compSums.[j]) = s) then
                let sumStr = _padStart (commatize (s)) (10) (" ")
                let piStr = _padStart (commatize (idx + 1)) (11) (" ")
                let ciStr = _padStart (commatize (j + 1)) (15) (" ")
                printfn "%s" (((((unbox<string> sumStr) + " | ") + (unbox<string> piStr)) + " | ") + (unbox<string> ciStr))
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
