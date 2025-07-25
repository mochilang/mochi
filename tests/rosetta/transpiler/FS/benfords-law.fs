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
let rec floorf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: int = int x
        __ret <- float y
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and fmtF3 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (floorf ((x * 1000.0) + 0.5))) / 1000.0
        let mutable s: string = string y
        let mutable dot: int = indexOf s "."
        if dot = (0 - 1) then
            s <- s + ".000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            if decs > 3 then
                s <- s.Substring(0, (dot + 4) - 0)
            else
                while decs < 3 do
                    s <- s + "0"
                    decs <- decs + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padFloat3 (x: float) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable width = width
    try
        let mutable s: string = fmtF3 x
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and fib1000 () =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    try
        let mutable a: float = 0.0
        let mutable b: float = 1.0
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < 1000 do
            res <- Array.append res [|b|]
            let mutable t: float = b
            b <- b + a
            a <- t
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and leadingDigit (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if x < 0.0 then
            x <- -x
        while x >= 10.0 do
            x <- x / 10.0
        while (x > 0.0) && (x < 1.0) do
            x <- x * 10.0
        __ret <- int x
        raise Return
        __ret
    with
        | Return -> __ret
and show (nums: float array) (title: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable nums = nums
    let mutable title = title
    try
        let mutable counts: int array = [|0; 0; 0; 0; 0; 0; 0; 0; 0|]
        for n in nums do
            let d: int = leadingDigit (float n)
            if (d >= 1) && (d <= 9) then
                counts.[d - 1] <- (int (counts.[d - 1])) + 1
        let preds: float array = [|0.301; 0.176; 0.125; 0.097; 0.079; 0.067; 0.058; 0.051; 0.046|]
        let total: int = Array.length nums
        printfn "%s" title
        printfn "%s" "Digit  Observed  Predicted"
        let mutable i: int = 0
        while i < 9 do
            let obs: float = (float (counts.[i])) / (float total)
            let mutable line: string = (((("  " + (string (i + 1))) + "  ") + (unbox<string> (padFloat3 obs 9))) + "  ") + (unbox<string> (padFloat3 (float (preds.[i])) 8))
            printfn "%s" line
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        show (unbox<float array> (fib1000())) "First 1000 Fibonacci numbers"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
