// Generated 2025-08-02 11:41 +0700

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

let rec getDivisors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable divs: int array = [|1; n|]
        let mutable i: int = 2
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                let j: int = int (n / i)
                divs <- Array.append divs [|i|]
                if i <> j then
                    divs <- Array.append divs [|j|]
            i <- i + 1
        __ret <- divs
        raise Return
        __ret
    with
        | Return -> __ret
and sum (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable s: int = 0
        for x in xs do
            s <- s + x
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and isPartSum (divs: int array) (target: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable divs = divs
    let mutable target = target
    try
        let mutable possible: bool array = [||]
        let mutable i: int = 0
        while i <= target do
            possible <- Array.append possible [|false|]
            i <- i + 1
        possible.[0] <- true
        for v in divs do
            let mutable s: int = target
            while s >= v do
                if possible.[s - v] then
                    possible.[s] <- true
                s <- s - 1
        __ret <- possible.[target]
        raise Return
        __ret
    with
        | Return -> __ret
and isZumkeller (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable divs: int array = getDivisors (n)
        let mutable s: int = Seq.sum (divs)
        if (((s % 2 + 2) % 2)) = 1 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 1 then
            let abundance: int = s - (2 * n)
            __ret <- (abundance > 0) && ((((abundance % 2 + 2) % 2)) = 0)
            raise Return
        __ret <- isPartSum (divs) (s / 2)
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string (n)
        while (String.length (s)) < width do
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
        printfn "%s" ("The first 220 Zumkeller numbers are:")
        let mutable count: int = 0
        let mutable line: string = ""
        let mutable i: int = 2
        while count < 220 do
            if isZumkeller (i) then
                line <- (line + (unbox<string> (pad (i) (3)))) + " "
                count <- count + 1
                if (((count % 20 + 20) % 20)) = 0 then
                    printfn "%s" (_substring line 0 ((String.length (line)) - 1))
                    line <- ""
            i <- i + 1
        printfn "%s" ("\nThe first 40 odd Zumkeller numbers are:")
        count <- 0
        line <- ""
        i <- 3
        while count < 40 do
            if isZumkeller (i) then
                line <- (line + (unbox<string> (pad (i) (5)))) + " "
                count <- count + 1
                if (((count % 10 + 10) % 10)) = 0 then
                    printfn "%s" (_substring line 0 ((String.length (line)) - 1))
                    line <- ""
            i <- i + 2
        printfn "%s" ("\nThe first 40 odd Zumkeller numbers which don't end in 5 are:")
        count <- 0
        line <- ""
        i <- 3
        while count < 40 do
            if ((((i % 10 + 10) % 10)) <> 5) && (unbox<bool> (isZumkeller (i))) then
                line <- (line + (unbox<string> (pad (i) (7)))) + " "
                count <- count + 1
                if (((count % 8 + 8) % 8)) = 0 then
                    printfn "%s" (_substring line 0 ((String.length (line)) - 1))
                    line <- ""
            i <- i + 2
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
