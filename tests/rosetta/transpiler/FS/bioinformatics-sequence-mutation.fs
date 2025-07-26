// Generated 2025-07-26 02:24 +0000

exception Return

let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec randInt (s: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    let mutable n = n
    try
        let next: int = ((((s * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and makeSeq (s: int) (le: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable s = s
    let mutable le = le
    try
        let bases: string = "ACGT"
        let mutable out: string = ""
        let mutable i: int = 0
        while i < le do
            let mutable r: int array = randInt s 4
            s <- unbox<int> (r.[0])
            let idx: int = unbox<int> (r.[1])
            out <- out + (_substring bases idx (idx + 1))
            i <- i + 1
        __ret <- [|box s; box out|]
        raise Return
        __ret
    with
        | Return -> __ret
and mutate (s: int) (dna: string) (w: int array) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable s = s
    let mutable dna = dna
    let mutable w = w
    try
        let bases: string = "ACGT"
        let le: int = String.length dna
        let mutable r: int array = randInt s le
        s <- unbox<int> (r.[0])
        let p: int = unbox<int> (r.[1])
        r <- unbox<int array> (randInt s 300)
        s <- unbox<int> (r.[0])
        let x: int = unbox<int> (r.[1])
        let mutable arr: string array = [||]
        let mutable i: int = 0
        while i < le do
            arr <- unbox<string array> (Array.append arr [|_substring dna i (i + 1)|])
            i <- i + 1
        if x < (unbox<int> (w.[0])) then
            r <- unbox<int array> (randInt s 4)
            s <- unbox<int> (r.[0])
            let idx: int = unbox<int> (r.[1])
            let b: string = _substring bases idx (idx + 1)
            printfn "%s" (((((("  Change @" + (unbox<string> (padLeft (string p) 3))) + " '") + (unbox<string> (arr.[p]))) + "' to '") + b) + "'")
            arr.[p] <- b
        else
            if x < (unbox<int> ((w.[0]) + (w.[1]))) then
                printfn "%s" (((("  Delete @" + (unbox<string> (padLeft (string p) 3))) + " '") + (unbox<string> (arr.[p]))) + "'")
                let mutable j: int = p
                while j < (unbox<int> ((unbox<int> (Array.length arr)) - 1)) do
                    arr.[j] <- arr.[j + 1]
                    j <- j + 1
                arr <- unbox<string array> (Array.sub arr 0 ((unbox<int> ((unbox<int> (Array.length arr)) - 1)) - 0))
            else
                r <- unbox<int array> (randInt s 4)
                s <- unbox<int> (r.[0])
                let idx2: int = unbox<int> (r.[1])
                let b: string = _substring bases idx2 (idx2 + 1)
                arr <- unbox<string array> (Array.append arr [|""|])
                let mutable j: int = (unbox<int> (Array.length arr)) - 1
                while j > p do
                    arr.[j] <- arr.[j - 1]
                    j <- j - 1
                printfn "%s" (((("  Insert @" + (unbox<string> (padLeft (string p) 3))) + " '") + b) + "'")
                arr.[p] <- b
        let mutable out: string = ""
        i <- 0
        while i < (unbox<int> (Array.length arr)) do
            out <- out + (unbox<string> (arr.[i]))
            i <- i + 1
        __ret <- [|box s; box out|]
        raise Return
        __ret
    with
        | Return -> __ret
and prettyPrint (dna: string) (rowLen: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable dna = dna
    let mutable rowLen = rowLen
    try
        printfn "%s" "SEQUENCE:"
        let le: int = String.length dna
        let mutable i: int = 0
        while i < le do
            let mutable k: int = i + rowLen
            if k > le then
                k <- le
            printfn "%s" (((unbox<string> (padLeft (string i) 5)) + ": ") + (dna.Substring(i, k - i)))
            i <- i + rowLen
        let mutable a: int = 0
        let mutable c: int = 0
        let mutable g: int = 0
        let mutable t: int = 0
        let mutable idx: int = 0
        while idx < le do
            let ch: string = _substring dna idx (idx + 1)
            if ch = "A" then
                a <- a + 1
            else
                if ch = "C" then
                    c <- c + 1
                else
                    if ch = "G" then
                        g <- g + 1
                    else
                        if ch = "T" then
                            t <- t + 1
            idx <- idx + 1
        printfn "%s" ""
        printfn "%s" "BASE COUNT:"
        printfn "%s" ("    A: " + (unbox<string> (padLeft (string a) 3)))
        printfn "%s" ("    C: " + (unbox<string> (padLeft (string c) 3)))
        printfn "%s" ("    G: " + (unbox<string> (padLeft (string g) 3)))
        printfn "%s" ("    T: " + (unbox<string> (padLeft (string t) 3)))
        printfn "%s" "    ------"
        printfn "%s" ("    Σ: " + (string le))
        printfn "%s" "    ======"
        __ret
    with
        | Return -> __ret
and wstring (w: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable w = w
    try
        __ret <- ((((("  Change: " + (string (w.[0]))) + "\n  Delete: ") + (string (w.[1]))) + "\n  Insert: ") + (string (w.[2]))) + "\n"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable seed: int = 1
        let mutable res: obj array = makeSeq seed 250
        seed <- unbox<int> (res.[0])
        let mutable dna: string = unbox<string> (res.[1])
        prettyPrint dna 50
        let muts: int = 10
        let w: int array = [|100; 100; 100|]
        printfn "%s" "\nWEIGHTS (ex 300):"
        printfn "%A" (wstring w)
        printfn "%s" (("MUTATIONS (" + (string muts)) + "):")
        let mutable i: int = 0
        while i < muts do
            res <- unbox<obj array> (mutate seed dna w)
            seed <- unbox<int> (res.[0])
            dna <- unbox<string> (res.[1])
            i <- i + 1
        printfn "%s" ""
        prettyPrint dna 50
        __ret
    with
        | Return -> __ret
main()
