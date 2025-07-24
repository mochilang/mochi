// Generated 2025-07-24 20:52 +0700

exception Return

let THRESHOLD: int = 140737488355328
let rec indexOf (xs: int array) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if (xs.[i]) = value then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and contains (xs: int array) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        __ret <- (indexOf xs value) <> (0 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and maxOf (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
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
and intSqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 0
            raise Return
        let mutable x: int = n
        let mutable y: int = (x + 1) / 2
        while y < x do
            x <- y
            y <- (x + (n / x)) / 2
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and sumProperDivisors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 2 then
            __ret <- 0
            raise Return
        let sqrt = intSqrt n
        let mutable sum: int = 1
        let mutable i: int = 2
        while i <= sqrt do
            if (n % i) = 0 then
                sum <- (sum + i) + (n / i)
            i <- i + 1
        if (sqrt * sqrt) = n then
            sum <- sum - sqrt
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and classifySequence (k: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable k = k
    try
        let mutable last: int = k
        let mutable seq: int array = [|k|]
        while true do
            last <- sumProperDivisors last
            seq <- Array.append seq [|last|]
            let n = Array.length seq
            let mutable aliquot: string = ""
            if last = 0 then
                aliquot <- "Terminating"
            else
                if (n = 2) && (last = k) then
                    aliquot <- "Perfect"
                else
                    if (n = 3) && (last = k) then
                        aliquot <- "Amicable"
                    else
                        if (n >= 4) && (last = k) then
                            aliquot <- ("Sociable[" + (string (n - 1))) + "]"
                        else
                            if last = (seq.[n - 2]) then
                                aliquot <- "Aspiring"
                            else
                                if contains (Array.sub seq 1 ((maxOf 1 (n - 2)) - 1)) last then
                                    let idx = indexOf seq last
                                    aliquot <- ("Cyclic[" + (string ((n - 1) - idx))) + "]"
                                else
                                    if (n = 16) || (last > THRESHOLD) then
                                        aliquot <- "Non-Terminating"
            if aliquot <> "" then
                __ret <- Map.ofList [("seq", box seq); ("aliquot", box aliquot)]
                raise Return
        __ret <- Map.ofList [("seq", box seq); ("aliquot", box "")]
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (n: int) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable w = w
    try
        let mutable s: string = string n
        while (String.length s) < w do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padRight (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable r: string = s
        while (String.length r) < w do
            r <- r + " "
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and joinWithCommas (seq: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable seq = seq
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length seq) do
            s <- s + (string (seq.[i]))
            if i < ((Seq.length seq) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        printfn "%s" "Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n"
        let mutable k: int = 1
        while k <= 10 do
            let res = classifySequence k
            printfn "%s" (((((padLeft k 2) + ": ") + (padRight ((res.["aliquot"]) :?> string) 15)) + " ") + (joinWithCommas ((res.["seq"]) :?> int array)))
            k <- k + 1
        printfn "%s" ""
        let s: int array = [|11; 12; 28; 496; 220; 1184; 12496; 1264460; 790; 909; 562; 1064; 1488|]
        let mutable i: int = 0
        while i < (Array.length s) do
            let ``val`` = s.[i]
            let res = classifySequence ``val``
            printfn "%s" (((((padLeft ``val`` 7) + ": ") + (padRight ((res.["aliquot"]) :?> string) 15)) + " ") + (joinWithCommas ((res.["seq"]) :?> int array)))
            i <- i + 1
        printfn "%s" ""
        let big: int = 15355717786080
        let r = classifySequence big
        printfn "%s" (((((string big) + ": ") + (padRight ((r.["aliquot"]) :?> string) 15)) + " ") + (joinWithCommas ((r.["seq"]) :?> int array)))
        __ret
    with
        | Return -> __ret
main()
