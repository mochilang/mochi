// Generated 2025-07-28 07:48 +0700

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
let rec newTerm (a: int) (b: int) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable a = a
    let mutable b = b
    try
        __ret <- Map.ofList [("a", a); ("b", b)]
        raise Return
        __ret
    with
        | Return -> __ret
and cfSqrt2 (nTerms: int) =
    let mutable __ret : Map<string, int> array = Unchecked.defaultof<Map<string, int> array>
    let mutable nTerms = nTerms
    try
        let mutable f: Map<string, int> array = [||]
        let mutable n: int = 0
        while n < nTerms do
            f <- Array.append f [|unbox<Map<string, int>> (newTerm 2 1)|]
            n <- n + 1
        if nTerms > 0 then
            f <- Map.add 0 (box (Map.add "a" 1 (f.[0]))) f
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and cfNap (nTerms: int) =
    let mutable __ret : Map<string, int> array = Unchecked.defaultof<Map<string, int> array>
    let mutable nTerms = nTerms
    try
        let mutable f: Map<string, int> array = [||]
        let mutable n: int = 0
        while n < nTerms do
            f <- Array.append f [|unbox<Map<string, int>> (newTerm n (n - 1))|]
            n <- n + 1
        if nTerms > 0 then
            f <- Map.add 0 (box (Map.add "a" 2 (f.[0]))) f
        if nTerms > 1 then
            f <- Map.add 1 (box (Map.add "b" 1 (f.[1]))) f
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and cfPi (nTerms: int) =
    let mutable __ret : Map<string, int> array = Unchecked.defaultof<Map<string, int> array>
    let mutable nTerms = nTerms
    try
        let mutable f: Map<string, int> array = [||]
        let mutable n: int = 0
        while n < nTerms do
            let g: int = (2 * n) - 1
            f <- Array.append f [|unbox<Map<string, int>> (newTerm 6 (g * g))|]
            n <- n + 1
        if nTerms > 0 then
            f <- Map.add 0 (box (Map.add "a" 3 (f.[0]))) f
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and real (f: Map<string, int> array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    try
        let mutable r: float = 0.0
        let mutable i: int = (Seq.length f) - 1
        while i > 0 do
            r <- (float (f.[i].["b"] |> unbox<int>)) / ((float (f.[i].["a"] |> unbox<int>)) + r)
            i <- i - 1
        if (Seq.length f) > 0 then
            r <- r + (float (f.[0].["a"] |> unbox<int>))
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("sqrt2: " + (string (real (cfSqrt2 20))))
        printfn "%s" ("nap:   " + (string (real (cfNap 20))))
        printfn "%s" ("pi:    " + (string (real (cfPi 20))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
