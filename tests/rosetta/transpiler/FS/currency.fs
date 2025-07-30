// Generated 2025-07-30 21:41 +0700

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

let rec parseIntDigits (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable n: int = 0
        let mutable i: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if not (Map.containsKey ch digits) then
                __ret <- 0
                raise Return
            n <- int ((n * 10) + (int (digits.[ch] |> unbox<int>)))
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and parseDC (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable neg: bool = false
        if ((String.length s) > 0) && ((s.Substring(0, 1 - 0)) = "-") then
            neg <- true
            s <- _substring s 1 (String.length s)
        let mutable dollars: int = 0
        let mutable cents: int = 0
        let mutable i: int = 0
        let mutable seenDot: bool = false
        let mutable centDigits: int = 0
        try
            while i < (String.length s) do
                try
                    let ch: string = s.Substring(i, (i + 1) - i)
                    if ch = "." then
                        seenDot <- true
                        i <- i + 1
                        raise Continue
                    let d: int = parseIntDigits ch
                    if seenDot then
                        if centDigits < 2 then
                            cents <- (cents * 10) + d
                            centDigits <- centDigits + 1
                    else
                        dollars <- (dollars * 10) + d
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if centDigits = 1 then
            cents <- cents * 10
        let mutable ``val``: int = (dollars * 100) + cents
        if neg then
            ``val`` <- -``val``
        __ret <- ``val``
        raise Return
        __ret
    with
        | Return -> __ret
and parseRate (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable neg: bool = false
        if ((String.length s) > 0) && ((s.Substring(0, 1 - 0)) = "-") then
            neg <- true
            s <- _substring s 1 (String.length s)
        let mutable whole: int = 0
        let mutable frac: int = 0
        let mutable digits: int = 0
        let mutable seenDot: bool = false
        let mutable i: int = 0
        try
            while i < (String.length s) do
                try
                    let ch: string = s.Substring(i, (i + 1) - i)
                    if ch = "." then
                        seenDot <- true
                        i <- i + 1
                        raise Continue
                    let d: int = parseIntDigits ch
                    if seenDot then
                        if digits < 4 then
                            frac <- (frac * 10) + d
                            digits <- digits + 1
                    else
                        whole <- (whole * 10) + d
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        while digits < 4 do
            frac <- frac * 10
            digits <- digits + 1
        let mutable ``val``: int = (whole * 10000) + frac
        if neg then
            ``val`` <- -``val``
        __ret <- ``val``
        raise Return
        __ret
    with
        | Return -> __ret
and dcString (dc: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable dc = dc
    try
        let mutable d: int = dc / 100
        let mutable n: int = dc
        if n < 0 then
            n <- -n
        let mutable c: int = ((n % 100 + 100) % 100)
        let mutable cstr: string = string c
        if (String.length cstr) = 1 then
            cstr <- "0" + cstr
        __ret <- ((string d) + ".") + cstr
        raise Return
        __ret
    with
        | Return -> __ret
and extend (dc: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable dc = dc
    let mutable n = n
    try
        __ret <- dc * n
        raise Return
        __ret
    with
        | Return -> __ret
and tax (total: int) (rate: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable total = total
    let mutable rate = rate
    try
        __ret <- int (((total * rate) + 5000) / 10000)
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        let mutable out: string = s
        while (String.length out) < n do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let hp: int = parseDC "5.50"
        let mp: int = parseDC "2.86"
        let rate: int = parseRate "0.0765"
        let totalBeforeTax: int = (extend hp (int 4000000000000000L)) + (extend mp 2)
        let t: int = tax totalBeforeTax rate
        let total: int = totalBeforeTax + t
        printfn "%s" ("Total before tax: " + (unbox<string> (padLeft (dcString totalBeforeTax) 22)))
        printfn "%s" ("             Tax: " + (unbox<string> (padLeft (dcString t) 22)))
        printfn "%s" ("           Total: " + (unbox<string> (padLeft (dcString total) 22)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
