// Generated 2025-07-25 10:58 +0000

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
        let mutable i: int64 = 0
        while i < (unbox<int> (Array.length xs)) do
            if (xs.[i]) = value then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and contains (xs: int64 array) (value: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        __ret <- (unbox<int> (indexOf xs value)) <> (0 - 1)
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
        if n = (unbox<int64> 0) then
            __ret <- 0
            raise Return
        let mutable x: int64 = n
        let mutable y = (unbox<int> (x + (unbox<int64> 1))) / 2
        while y < x do
            x <- y
            y <- (x + (n / x)) / (unbox<int64> 2)
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and sumProperDivisors (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        if n < (unbox<int64> 2) then
            __ret <- 0
            raise Return
        let sqrt: int64 = intSqrt n
        let mutable sum: int64 = 1
        let mutable i: int64 = 2
        while (unbox<int64> i) <= sqrt do
            if (unbox<int> (n % (unbox<int64> i))) = 0 then
                sum <- (sum + i) + (unbox<int> (n / (unbox<int64> i)))
            i <- i + 1
        if (sqrt * sqrt) = n then
            sum <- (unbox<int64> sum) - sqrt
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
            let n: int64 = Array.length seq
            let mutable aliquot: string = ""
            if last = (unbox<int64> 0) then
                aliquot <- "Terminating"
            else
                if (n = (unbox<int64> 2)) && (last = k) then
                    aliquot <- "Perfect"
                else
                    if (n = (unbox<int64> 3)) && (last = k) then
                        aliquot <- "Amicable"
                    else
                        if (n >= (unbox<int64> 4)) && (last = k) then
                            aliquot <- ("Sociable[" + (string (n - (unbox<int64> 1)))) + "]"
                        else
                            if last = (seq.[n - (unbox<int64> 2)]) then
                                aliquot <- "Aspiring"
                            else
                                if contains (Array.sub seq 1 ((unbox<int> (maxOf 1 (unbox<int64> (n - (unbox<int64> 2))))) - 1)) last then
                                    let idx: int64 = indexOf seq last
                                    aliquot <- ("Cyclic[" + (string ((n - (unbox<int64> 1)) - idx))) + "]"
                                else
                                    if (n = (unbox<int64> 16)) || (last > THRESHOLD) then
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
        while (unbox<int64> (String.length s)) < w do
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
        while (unbox<int64> (String.length r)) < w do
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
        let mutable i: int64 = 0
        while i < (unbox<int> (Array.length seq)) do
            s <- s + (string (seq.[i]))
            if i < (unbox<int> ((unbox<int> (Array.length seq)) - 1)) then
                s <- s + ", "
            i <- i + 1
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
        let mutable k: int64 = 1
        while k <= 10 do
            let res: Map<string, obj> = classifySequence k
            printfn "%s" (((((unbox<string> (padLeft k 2)) + ": ") + (unbox<string> (padRight (unbox<string> (res.["aliquot"])) 15))) + " ") + (unbox<string> (joinWithCommas (unbox<int64 array> (res.["seq"])))))
            k <- k + 1
        printfn "%s" ""
        let s: int array = [|11; 12; 28; 496; 220; 1184; 12496; 1264460; 790; 909; 562; 1064; 1488|]
        let mutable i: int64 = 0
        while i < (unbox<int> (Array.length s)) do
            let ``val``: int64 = s.[i]
            let res: Map<string, obj> = classifySequence ``val``
            printfn "%s" (((((unbox<string> (padLeft ``val`` 7)) + ": ") + (unbox<string> (padRight (unbox<string> (res.["aliquot"])) 15))) + " ") + (unbox<string> (joinWithCommas (unbox<int64 array> (res.["seq"])))))
            i <- i + 1
        printfn "%s" ""
        let big: int64 = 15355717786080L
        let r: Map<string, obj> = classifySequence big
        printfn "%s" (((((string big) + ": ") + (unbox<string> (padRight (unbox<string> (r.["aliquot"])) 15))) + " ") + (unbox<string> (joinWithCommas (unbox<int64 array> (r.["seq"])))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
