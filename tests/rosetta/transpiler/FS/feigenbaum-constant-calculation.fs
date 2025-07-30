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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and fmt8 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (floorf ((x * 100000000.0) + 0.5))) / 100000000.0
        let mutable s: string = string y
        let mutable dot: int = s.IndexOf(".")
        if dot = (0 - 1) then
            s <- s + ".00000000"
        else
            let mutable decs: int = ((String.length s) - dot) - 1
            while decs < 8 do
                s <- s + "0"
                decs <- decs + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad2 (x: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable s: string = string x
        if (String.length s) < 2 then
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
        let maxIt: int = 13
        let maxItJ: int = 10
        let mutable a1: float = 1.0
        let mutable a2: float = 0.0
        let mutable d1: float = 3.2
        printfn "%s" " i       d"
        let mutable i: int = 2
        while i <= maxIt do
            let mutable a: float = a1 + ((a1 - a2) / d1)
            let mutable j: int = 1
            while j <= maxItJ do
                let mutable x: float = 0.0
                let mutable y: float = 0.0
                let mutable k: int = 1
                let limit: int = pow_int 2 i
                while k <= limit do
                    y <- 1.0 - ((2.0 * y) * x)
                    x <- a - (x * x)
                    k <- k + 1
                a <- a - (x / y)
                j <- j + 1
            let mutable d: float = (a1 - a2) / (a - a1)
            printfn "%s" (((unbox<string> (pad2 i)) + "    ") + (unbox<string> (fmt8 d)))
            d1 <- d
            a2 <- a1
            a1 <- a
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
and pow_int (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable r: int = 1
        let mutable b: int = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                r <- r * b
            b <- b * b
            e <- int (e / 2)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
main()
