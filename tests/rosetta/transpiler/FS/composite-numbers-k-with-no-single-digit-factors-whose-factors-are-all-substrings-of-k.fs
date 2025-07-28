// Generated 2025-07-28 07:48 +0700

exception Break
exception Continue

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

let rec primeFactors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable factors: int array = [||]
        let mutable x: int = n
        while (((x % 2 + 2) % 2)) = 0 do
            factors <- Array.append factors [|2|]
            x <- int (x / 2)
        let mutable p: int = 3
        while (p * p) <= x do
            while (((x % p + p) % p)) = 0 do
                factors <- Array.append factors [|p|]
                x <- int (x / p)
            p <- p + 2
        if x > 1 then
            factors <- Array.append factors [|x|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable out: string = ""
        let mutable i: int = (String.length s) - 1
        let mutable c: int = 0
        while i >= 0 do
            out <- (_substring s i (i + 1)) + out
            c <- c + 1
            if ((((c % 3 + 3) % 3)) = 0) && (i > 0) then
                out <- "," + out
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (sub: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    try
        let mutable i: int = 0
        while (i + (String.length sub)) <= (String.length s) do
            if (_substring s i (i + (String.length sub))) = sub then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and pad10 (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable str: string = s
        while (String.length str) < 10 do
            str <- " " + str
        __ret <- str
        raise Return
        __ret
    with
        | Return -> __ret
and trimRightStr (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable ``end``: int = String.length s
        while (``end`` > 0) && ((_substring s (``end`` - 1) ``end``) = " ") do
            ``end`` <- ``end`` - 1
        __ret <- _substring s 0 ``end``
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable res: int array = [||]
        let mutable count: int = 0
        let mutable k: int = 11 * 11
        try
            while count < 20 do
                try
                    if (((((k % 3 + 3) % 3)) = 0) || ((((k % 5 + 5) % 5)) = 0)) || ((((k % 7 + 7) % 7)) = 0) then
                        k <- k + 2
                        raise Continue
                    let factors: int array = primeFactors k
                    if (Seq.length factors) > 1 then
                        let s: string = string k
                        let mutable includesAll: bool = true
                        let mutable prev: int = -1
                        try
                            for f in factors do
                                try
                                    if f = prev then
                                        raise Continue
                                    let fs: string = string f
                                    if (int (indexOf s fs)) = (-1) then
                                        includesAll <- false
                                        raise Break
                                    prev <- f
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        if includesAll then
                            res <- Array.append res [|k|]
                            count <- count + 1
                    k <- k + 2
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable line: string = ""
        for e in Array.sub res 0 (10 - 0) do
            line <- (line + (unbox<string> (pad10 (commatize e)))) + " "
        printfn "%s" (trimRightStr line)
        line <- ""
        for e in Array.sub res 10 (20 - 10) do
            line <- (line + (unbox<string> (pad10 (commatize e)))) + " "
        printfn "%s" (trimRightStr line)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
