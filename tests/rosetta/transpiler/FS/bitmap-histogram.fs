// Generated 2025-07-26 04:38 +0700

exception Return

let rec image () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        __ret <- [|[|0; 0; 10000|]; [|65535; 65535; 65535|]; [|65535; 65535; 65535|]|]
        raise Return
        __ret
    with
        | Return -> __ret
and histogram (g: int array array) (bins: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable bins = bins
    try
        if bins <= 0 then
            bins <- Seq.length (g.[0])
        let mutable h: int array = [||]
        let mutable i: int = 0
        while i < bins do
            h <- Array.append h [|0|]
            i <- i + 1
        let mutable y: int = 0
        while y < (int (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable x: int = 0
            while x < (int (Array.length row)) do
                let mutable p: int = row.[x]
                let mutable idx: int = int ((p * (bins - 1)) / 65535)
                h.[idx] <- (int (h.[idx])) + 1
                x <- x + 1
            y <- y + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and medianThreshold (h: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    try
        let mutable lb: int = 0
        let mutable ub: int = (int (Array.length h)) - 1
        let mutable lSum: int = 0
        let mutable uSum: int = 0
        while lb <= ub do
            if (lSum + (int (h.[lb]))) < (uSum + (int (h.[ub]))) then
                lSum <- lSum + (int (h.[lb]))
                lb <- lb + 1
            else
                uSum <- uSum + (int (h.[ub]))
                ub <- ub - 1
        __ret <- int ((ub * 65535) / (int (Array.length h)))
        raise Return
        __ret
    with
        | Return -> __ret
and threshold (g: int array array) (t: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable g = g
    let mutable t = t
    try
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < (int (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable newRow: int array = [||]
            let mutable x: int = 0
            while x < (int (Array.length row)) do
                if (int (row.[x])) < t then
                    newRow <- Array.append newRow [|0|]
                else
                    newRow <- Array.append newRow [|65535|]
                x <- x + 1
            out <- Array.append out [|newRow|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and printImage (g: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        let mutable y: int = 0
        while y < (int (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (int (Array.length row)) do
                if (int (row.[x])) = 0 then
                    line <- line + "0"
                else
                    line <- line + "1"
                x <- x + 1
            printfn "%s" line
            y <- y + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let img: int array array = image()
        let h: int array = histogram img 0
        printfn "%s" ("Histogram: " + (string h))
        let t: int = medianThreshold h
        printfn "%s" ("Threshold: " + (string t))
        let bw: int array array = threshold img t
        printImage bw
        __ret
    with
        | Return -> __ret
main()
