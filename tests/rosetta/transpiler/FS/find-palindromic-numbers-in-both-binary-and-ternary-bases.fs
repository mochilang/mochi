// Generated 2025-08-05 01:26 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec toBase (n: int) (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable b = b
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable s: string = ""
        let mutable x: int = n
        while x > 0 do
            s <- (string (((x % b + b) % b))) + s
            x <- int (x / b)
        __ret <- s
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
        if ((String.length(str)) > 0) && ((string (str.[0])) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        while i < (String.length(str)) do
            n <- ((n * 10) + (int (str.Substring(i, (i + 1) - i)))) - (int "0")
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntBase (s: string) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable b = b
    try
        let mutable n: int = 0
        let mutable i: int = 0
        while i < (String.length(s)) do
            n <- (n * b) + (parseIntStr (s.Substring(i, (i + 1) - i)))
            i <- i + 1
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
        let mutable i: int = (String.length(s)) - 1
        while i >= 0 do
            out <- out + (s.Substring(i, (i + 1) - i))
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and isPalindrome (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        __ret <- s = (reverseStr (s))
        raise Return
        __ret
    with
        | Return -> __ret
and isPalindromeBin (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let b: string = toBase (n) (2)
        __ret <- isPalindrome (b)
        raise Return
        __ret
    with
        | Return -> __ret
and myMin (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and myMax (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and reverse3 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable x: int = 0
        let mutable y: int = n
        while y <> 0 do
            x <- (x * 3) + (((y % 3 + 3) % 3))
            y <- int (y / 3)
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and show (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        printfn "%s" ("Decimal : " + (string (n)))
        printfn "%s" ("Binary  : " + (toBase (n) (2)))
        printfn "%s" ("Ternary : " + (toBase (n) (3)))
        printfn "%s" ("")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("The first 6 numbers which are palindromic in both binary and ternary are :\n")
        show (0)
        let mutable count: int = 1
        let mutable lo: int = 0
        let mutable hi: int = 1
        let mutable pow2: int = 1
        let mutable pow3: int = 1
        try
            while true do
                try
                    let mutable i: int = lo
                    while i < hi do
                        let mutable n: int = (((i * 3) + 1) * pow3) + (reverse3 (i))
                        if isPalindromeBin (n) then
                            show (n)
                            count <- count + 1
                            if count >= 6 then
                                __ret <- ()
                                raise Return
                        i <- i + 1
                    if i = pow3 then
                        pow3 <- pow3 * 3
                    else
                        pow2 <- pow2 * 4
                    try
                        while true do
                            try
                                while pow2 <= pow3 do
                                    pow2 <- pow2 * 4
                                let mutable lo2: int = int (((pow2 / pow3) - 1) / 3)
                                let mutable hi2: int = (int ((((pow2 * 2) / pow3) - 1) / 3)) + 1
                                let mutable lo3: int = int (pow3 / 3)
                                let mutable hi3: int = pow3
                                if lo2 >= hi3 then
                                    pow3 <- pow3 * 3
                                else
                                    if lo3 >= hi2 then
                                        pow2 <- pow2 * 4
                                    else
                                        lo <- myMax (lo2) (lo3)
                                        hi <- myMin (hi2) (hi3)
                                        raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
