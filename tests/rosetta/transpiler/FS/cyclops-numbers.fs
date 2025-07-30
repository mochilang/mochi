// Generated 2025-07-30 21:41 +0700

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

let rec digits (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        let mutable rev: int array = [||]
        let mutable x: int = n
        while x > 0 do
            rev <- Array.append rev [|((x % 10 + 10) % 10)|]
            x <- int (x / 10)
        let mutable out: int array = [||]
        let mutable i: int = (Seq.length rev) - 1
        while i >= 0 do
            out <- Array.append out [|rev.[i]|]
            i <- i - 1
        __ret <- out
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
        let mutable i: int = String.length s
        while i > 3 do
            out <- ("," + (s.Substring(i - 3, i - (i - 3)))) + out
            i <- i - 3
        out <- (s.Substring(0, i - 0)) + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if ((i + (String.length sep)) <= (String.length s)) && ((_substring s i (i + (String.length sep))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (s.Substring(i, (i + 1) - i))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and reverseStr (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = (String.length s) - 1
        while i >= 0 do
            out <- out + (s.Substring(i, (i + 1) - i))
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable out: string = s
        while (String.length out) < w do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and findFirst (list: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list = list
    try
        let mutable i: int = 0
        while i < (Seq.length list) do
            if (list.[i]) > 10000000 then
                __ret <- unbox<int array> [|list.[i]; i|]
                raise Return
            i <- i + 1
        __ret <- unbox<int array> [|-1; -1|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ranges: int array array = [|[|0; 0|]; [|101; 909|]; [|11011; 99099|]; [|1110111; 9990999|]; [|111101111; 119101111|]|]
        let mutable cyclops: int array = [||]
        for r in ranges do
            let start: int = r.[0]
            let ``end``: int = r.[1]
            let numDigits: int = String.length (string start)
            let center: int = numDigits / 2
            let mutable i: int = start
            while i <= ``end`` do
                let ds: int array = digits i
                if (ds.[center]) = 0 then
                    let mutable count: int = 0
                    for d in ds do
                        if d = 0 then
                            count <- count + 1
                    if count = 1 then
                        cyclops <- Array.append cyclops [|i|]
                i <- i + 1
        printfn "%s" "The first 50 cyclops numbers are:"
        let mutable idx: int = 0
        while idx < 50 do
            printfn "%s" ((unbox<string> (pad (commatize (cyclops.[idx])) 6)) + " ")
            idx <- idx + 1
            if (((idx % 10 + 10) % 10)) = 0 then
                printfn "%s" "\n"
        let fi: int array = findFirst cyclops
        printfn "%s" ((("\nFirst such number > 10 million is " + (unbox<string> (commatize (fi.[0])))) + " at zero-based index ") + (unbox<string> (commatize (fi.[1]))))
        let mutable primes: int array = [||]
        for n in cyclops do
            if isPrime n then
                primes <- Array.append primes [|n|]
        printfn "%s" "\n\nThe first 50 prime cyclops numbers are:"
        idx <- 0
        while idx < 50 do
            printfn "%s" ((unbox<string> (pad (commatize (primes.[idx])) 6)) + " ")
            idx <- idx + 1
            if (((idx % 10 + 10) % 10)) = 0 then
                printfn "%s" "\n"
        let fp: int array = findFirst primes
        printfn "%s" ((("\nFirst such number > 10 million is " + (unbox<string> (commatize (fp.[0])))) + " at zero-based index ") + (unbox<string> (commatize (fp.[1]))))
        let mutable bpcyclops: int array = [||]
        let mutable ppcyclops: int array = [||]
        for p in primes do
            let ps: string = string p
            let splitp: string array = ps.Split([|"0"|], System.StringSplitOptions.None)
            let noMiddle: int = parseIntStr ((splitp.[0]) + (splitp.[1]))
            if isPrime noMiddle then
                bpcyclops <- Array.append bpcyclops [|p|]
            if ps = (unbox<string> (reverseStr ps)) then
                ppcyclops <- Array.append ppcyclops [|p|]
        printfn "%s" "\n\nThe first 50 blind prime cyclops numbers are:"
        idx <- 0
        while idx < 50 do
            printfn "%s" ((unbox<string> (pad (commatize (bpcyclops.[idx])) 6)) + " ")
            idx <- idx + 1
            if (((idx % 10 + 10) % 10)) = 0 then
                printfn "%s" "\n"
        let fb: int array = findFirst bpcyclops
        printfn "%s" ((("\nFirst such number > 10 million is " + (unbox<string> (commatize (fb.[0])))) + " at zero-based index ") + (unbox<string> (commatize (fb.[1]))))
        printfn "%s" "\n\nThe first 50 palindromic prime cyclops numbers are:"
        idx <- 0
        while idx < 50 do
            printfn "%s" ((unbox<string> (pad (commatize (ppcyclops.[idx])) 9)) + " ")
            idx <- idx + 1
            if (((idx % 8 + 8) % 8)) = 0 then
                printfn "%s" "\n"
        let fpp: int array = findFirst ppcyclops
        printfn "%s" ((("\n\nFirst such number > 10 million is " + (unbox<string> (commatize (fpp.[0])))) + " at zero-based index ") + (unbox<string> (commatize (fpp.[1]))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
