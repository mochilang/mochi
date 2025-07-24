// Generated 2025-07-24 20:52 +0700

exception Return

let rec sortRunes (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable arr: string array = [||]
        let mutable i: int = 0
        while i < (String.length s) do
            arr <- Array.append arr [|s.Substring(i, (i + 1) - i)|]
            i <- i + 1
        let mutable n = Array.length arr
        let mutable m: int = 0
        while m < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let tmp = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- tmp
                j <- j + 1
            m <- m + 1
        let mutable out: string = ""
        i <- 0
        while i < n do
            out <- out + (arr.[i])
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and sortStrings (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = [||]
        let mutable tmp = xs
        while (Seq.length tmp) > 0 do
            let mutable min = tmp.[0]
            let mutable idx: int = 0
            let mutable i: int = 1
            while i < (Seq.length tmp) do
                if (tmp.[i]) < min then
                    min <- tmp.[i]
                    idx <- i
                i <- i + 1
            res <- Array.append res [|min|]
            let mutable out: string array = [||]
            let mutable j: int = 0
            while j < (Seq.length tmp) do
                if j <> idx then
                    out <- Array.append out [|tmp.[j]|]
                j <- j + 1
            tmp <- out
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let words: string array = [|"abel"; "able"; "bale"; "bela"; "elba"; "alger"; "glare"; "lager"; "large"; "regal"; "angel"; "angle"; "galen"; "glean"; "lange"; "caret"; "carte"; "cater"; "crate"; "trace"; "elan"; "lane"; "lean"; "lena"; "neal"; "evil"; "levi"; "live"; "veil"; "vile"|]
        let mutable groups: Map<string, string array> = Map.ofList []
        let mutable maxLen: int = 0
        for w in words do
            let k = sortRunes w
            if not (Map.containsKey k groups) then
                groups <- Map.add k [|w|] groups
            else
                groups <- Map.add k (Array.append groups.[k] [|w|]) groups
            if (Seq.length (groups.[k])) > maxLen then
                maxLen <- Seq.length (groups.[k])
        let mutable printed: Map<string, bool> = Map.ofList []
        for w in words do
            let k = sortRunes w
            if (Seq.length (groups.[k])) = maxLen then
                if not (Map.containsKey k printed) then
                    let mutable g = sortStrings (groups.[k])
                    let mutable line: string = "[" + (g.[0])
                    let mutable i: int = 1
                    while i < (Seq.length g) do
                        line <- (line + " ") + (g.[i])
                        i <- i + 1
                    line <- line + "]"
                    printfn "%s" line
                    printed <- Map.add k true printed
        __ret
    with
        | Return -> __ret
main()
