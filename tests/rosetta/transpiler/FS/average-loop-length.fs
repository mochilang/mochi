// Generated 2025-07-25 17:34 +0000

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
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and floorf (x: float) =
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
and fmtF (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (floorf ((x * 10000.0) + 0.5))) / 10000.0
        let mutable s: string = string y
        let mutable dot: int = indexOf s "."
        if dot = (0 - 1) then
            s <- s + ".0000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            if decs > 4 then
                s <- s.Substring(0, (dot + 5) - 0)
            else
                while decs < 4 do
                    s <- s + "0"
                    decs <- decs + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padInt (n: int) (width: int) =
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
and padFloat (x: float) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable width = width
    try
        let mutable s: string = fmtF x
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and avgLen (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let tests: int = 10000
        let mutable sum: int = 0
        let mutable seed: int = 1
        let mutable t: int = 0
        while t < tests do
            let mutable visited: bool array = [||]
            let mutable i: int = 0
            while i < n do
                visited <- Array.append visited [|false|]
                i <- i + 1
            let mutable x: int = 0
            while not (visited.[x]) do
                visited.[x] <- true
                sum <- sum + 1
                seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
                x <- ((seed % n + n) % n)
            t <- t + 1
        __ret <- (float sum) / (float tests)
        raise Return
        __ret
    with
        | Return -> __ret
and ana (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable nn: float = float n
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: float = nn - 1.0
        while i >= 1.0 do
            term <- term * (i / nn)
            sum <- sum + term
            i <- i - 1.0
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nmax: int = 20
        printfn "%s" " N    average    analytical    (error)"
        printfn "%s" "===  =========  ============  ========="
        let mutable n: int = 1
        while n <= nmax do
            let a: float = avgLen n
            let b: float = ana n
            let err = (float ((float (absf (a - b))) / b)) * 100.0
            let mutable line: string = (((((((unbox<string> (padInt n 3)) + "  ") + (unbox<string> (padFloat a 9))) + "  ") + (unbox<string> (padFloat b 12))) + "  (") + (unbox<string> (padFloat (float err) 6))) + "%)"
            printfn "%s" line
            n <- n + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
