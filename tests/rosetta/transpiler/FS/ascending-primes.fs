// Generated 2025-07-25 22:14 +0700

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
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (n % 2) = 0 then
            __ret <- n = 2
            raise Return
        if (n % 3) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (n % d) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (n % d) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let mutable asc: int array = [||]
let rec gen (first: int) (cand: int) (digits: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable first = first
    let mutable cand = cand
    let mutable digits = digits
    try
        if digits = 0 then
            if isPrime cand then
                asc <- Array.append asc [|cand|]
            __ret <- ()
            raise Return
        let mutable i: int = first
        while i < 10 do
            gen (i + 1) ((cand * 10) + i) (digits - 1)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
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
        let mutable digits: int = 1
        while digits < 10 do
            gen 1 0 digits
            digits <- digits + 1
        printfn "%s" (("There are " + (string (Array.length asc))) + " ascending primes, namely:")
        let mutable i: int = 0
        let mutable line: string = ""
        while i < (int (Array.length asc)) do
            line <- (line + (unbox<string> (pad (int (asc.[i])) 8))) + " "
            if ((i + 1) % 10) = 0 then
                printfn "%s" (line.Substring(0, ((String.length line) - 1) - 0))
                line <- ""
            i <- i + 1
        if (String.length line) > 0 then
            printfn "%s" (line.Substring(0, ((String.length line) - 1) - 0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
