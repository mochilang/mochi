// Generated 2025-07-31 00:10 +0700

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
let rec dbRec (k: int) (n: int) (t: int) (p: int) (a: int array) (seq: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable k = k
    let mutable n = n
    let mutable t = t
    let mutable p = p
    let mutable a = a
    let mutable seq = seq
    try
        if t > n then
            if (((n % p + p) % p)) = 0 then
                let mutable j: int = 1
                while j <= p do
                    seq <- Array.append seq [|a.[j]|]
                    j <- j + 1
        else
            a.[t] <- a.[t - p]
            seq <- dbRec k n (t + 1) p a seq
            let mutable j: int = (a.[t - p]) + 1
            while j < k do
                a.[t] <- j
                seq <- dbRec k n (t + 1) t a seq
                j <- j + 1
        __ret <- seq
        raise Return
        __ret
    with
        | Return -> __ret
and deBruijn (k: int) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable k = k
    let mutable n = n
    try
        let digits: string = "0123456789"
        let mutable alphabet: string = digits
        if k < 10 then
            alphabet <- digits.Substring(0, k - 0)
        let mutable a: int array = [||]
        let mutable i: int = 0
        while i < (k * n) do
            a <- Array.append a [|0|]
            i <- i + 1
        let mutable seq: int array = [||]
        seq <- dbRec k n 1 1 a seq
        let mutable b: string = ""
        let mutable idx: int = 0
        while idx < (Seq.length seq) do
            b <- b + (string (alphabet.[seq.[idx]]))
            idx <- idx + 1
        b <- b + (b.Substring(0, (n - 1) - 0))
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and allDigits (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if (ch < "0") || (ch > "9") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length str) do
            n <- (n * 10) + (int (str.Substring(i, (i + 1) - i)))
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and validate (db: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable db = db
    try
        let le: int = String.length db
        let mutable found: int array = [||]
        let mutable i: int = 0
        while i < 10000 do
            found <- Array.append found [|0|]
            i <- i + 1
        let mutable j: int = 0
        while j < (le - 3) do
            let mutable s: string = db.Substring(j, (j + 4) - j)
            if allDigits s then
                let mutable n: int = parseIntStr s
                found.[n] <- (found.[n]) + 1
            j <- j + 1
        let mutable errs: string array = [||]
        let mutable k: int = 0
        while k < 10000 do
            if (found.[k]) = 0 then
                errs <- Array.append errs [|("    PIN number " + (unbox<string> (padLeft k 4))) + " missing"|]
            else
                if (found.[k]) > 1 then
                    errs <- Array.append errs [|((("    PIN number " + (unbox<string> (padLeft k 4))) + " occurs ") + (string (found.[k]))) + " times"|]
            k <- k + 1
        let lerr: int = Seq.length errs
        if lerr = 0 then
            printfn "%s" "  No errors found"
        else
            let mutable pl: string = "s"
            if lerr = 1 then
                pl <- ""
            printfn "%s" (((("  " + (string lerr)) + " error") + pl) + " found:")
            let msg: string = joinStr errs "\n"
            printfn "%s" msg
        __ret
    with
        | Return -> __ret
and padLeft (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- "0" + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and joinStr (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and reverse (s: string) =
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
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable db: string = deBruijn 10 4
        let le: int = String.length db
        printfn "%s" ("The length of the de Bruijn sequence is " + (string le))
        printfn "%s" "\nThe first 130 digits of the de Bruijn sequence are:"
        printfn "%s" (db.Substring(0, 130 - 0))
        printfn "%s" "\nThe last 130 digits of the de Bruijn sequence are:"
        printfn "%s" (db.Substring(le - 130, (String.length db) - (le - 130)))
        printfn "%s" "\nValidating the de Bruijn sequence:"
        validate db
        printfn "%s" "\nValidating the reversed de Bruijn sequence:"
        let dbr: string = reverse db
        validate dbr
        db <- ((db.Substring(0, 4443 - 0)) + ".") + (db.Substring(4444, (String.length db) - 4444))
        printfn "%s" "\nValidating the overlaid de Bruijn sequence:"
        validate db
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
