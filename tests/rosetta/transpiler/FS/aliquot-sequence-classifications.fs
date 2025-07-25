// Generated 2025-07-25 21:49 +0700

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
let THRESHOLD: int64 = 140737488355328L
let rec indexOf (xs: int64 array) (value: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable i: int64 = 0L
        while i < (int64 (Array.length xs)) do
            if (xs.[int i]) = value then
                __ret <- i
                raise Return
            i <- i + 1L
        __ret <- 0L - 1L
        raise Return
        __ret
    with
        | Return -> __ret
and contains (xs: int64 array) (value: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        __ret <- (indexOf xs value) <> (0L - 1L)
        raise Return
        __ret
    with
        | Return -> __ret
and maxOf (a: int64) (b: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable a = a
    let mutable b = b
    try
        if a > b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
and intSqrt (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        if n = 0L then
            __ret <- 0L
            raise Return
        let mutable x: int64 = n
        let mutable y: int64 = (x + 1L) / 2L
        while y < x do
            x <- y
            y <- (x + (n / x)) / 2L
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and sumProperDivisors (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        if n < 2L then
            __ret <- 0L
            raise Return
        let sqrt: int64 = intSqrt n
        let mutable sum: int64 = 1L
        let mutable i: int64 = 2L
        while i <= sqrt do
            if (n % i) = 0L then
                sum <- (sum + i) + (n / i)
            i <- i + 1L
        if (sqrt * sqrt) = n then
            sum <- sum - sqrt
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and classifySequence (k: int64) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable k = k
    try
        let mutable last: int64 = k
        let mutable seq: int64 array = [|k|]
        while true do
            last <- sumProperDivisors last
            seq <- Array.append seq [|last|]
            let n: int64 = int64 (Array.length seq)
            let mutable aliquot: string = ""
            if last = 0L then
                aliquot <- "Terminating"
            else
                if (n = 2L) && (last = k) then
                    aliquot <- "Perfect"
                else
                    if (n = 3L) && (last = k) then
                        aliquot <- "Amicable"
                    else
                        if (n >= 4L) && (last = k) then
                            aliquot <- ("Sociable[" + (string (n - 1L))) + "]"
                        else
                            if last = (seq.[int (n - 2L)]) then
                                aliquot <- "Aspiring"
                            else
                                if contains (Array.sub seq (int 1L) (int ((maxOf 1L (n - 2L)) - 1L))) last then
                                    let idx: int64 = indexOf seq last
                                    aliquot <- ("Cyclic[" + (string ((n - 1L) - idx))) + "]"
                                else
                                    if (n = 16L) || (last > THRESHOLD) then
                                        aliquot <- "Non-Terminating"
            if aliquot <> "" then
                __ret <- Map.ofList [("seq", box seq); ("aliquot", box aliquot)]
                raise Return
        __ret <- Map.ofList [("seq", box seq); ("aliquot", box "")]
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (n: int64) (w: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable w = w
    try
        let mutable s: string = string n
        while (int64 (String.length s)) < w do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padRight (s: string) (w: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable r: string = s
        while (int64 (String.length r)) < w do
            r <- r + " "
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and joinWithCommas (seq: int64 array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable seq = seq
    try
        let mutable s: string = "["
        let mutable i: int64 = 0L
        while i < (int64 (Array.length seq)) do
            s <- s + (string (seq.[int i]))
            if i < ((int64 (Array.length seq)) - 1L) then
                s <- s + ", "
            i <- i + 1L
        s <- s + "]"
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
        printfn "%s" "Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n"
        let mutable k: int64 = 1L
        while k <= 10L do
            let res: Map<string, obj> = classifySequence k
            printfn "%s" (((((padLeft k 2L) + ": ") + (padRight (unbox<string> (res.["aliquot"])) 15L)) + " ") + (joinWithCommas (unbox<int64 array> (res.["seq"]))))
            k <- k + 1L
        printfn "%s" ""
        let s: int64 array = [|11L; 12L; 28L; 496L; 220L; 1184L; 12496L; 1264460L; 790L; 909L; 562L; 1064L; 1488L|]
        let mutable i: int64 = 0L
        while i < (int64 (Array.length s)) do
            let ``val``: int64 = s.[int i]
            let res: Map<string, obj> = classifySequence ``val``
            printfn "%s" (((((padLeft ``val`` 7L) + ": ") + (padRight (unbox<string> (res.["aliquot"])) 15L)) + " ") + (joinWithCommas (unbox<int64 array> (res.["seq"]))))
            i <- i + 1L
        printfn "%s" ""
        let big: int64 = 15355717786080L
        let r: Map<string, obj> = classifySequence big
        printfn "%s" (((((string big) + ": ") + (padRight (unbox<string> (r.["aliquot"])) 15L)) + " ") + (joinWithCommas (unbox<int64 array> (r.["seq"]))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
