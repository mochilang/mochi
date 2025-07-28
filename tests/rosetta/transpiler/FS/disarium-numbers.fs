// Generated 2025-07-28 10:03 +0700

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
let rec pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and isDisarium (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable digits: int array = [||]
        let mutable x: int = n
        if x = 0 then
            digits <- Array.append digits [|0|]
        while x > 0 do
            digits <- Array.append digits [|((x % 10 + 10) % 10)|]
            x <- int (x / 10)
        let mutable sum: int = 0
        let mutable pos: int = 1
        let mutable i: int = (Seq.length digits) - 1
        while i >= 0 do
            sum <- sum + (int (pow (digits.[i]) pos))
            pos <- pos + 1
            i <- i - 1
        __ret <- sum = n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable count: int = 0
        let mutable n: int = 0
        while (count < 19) && (n < 3000000) do
            if isDisarium n then
                printfn "%s" (string n)
                count <- count + 1
            n <- n + 1
        printfn "%s" (("\nFound the first " + (string count)) + " Disarium numbers.")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
