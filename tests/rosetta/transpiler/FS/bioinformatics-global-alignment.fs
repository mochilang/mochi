// Generated 2025-07-26 04:38 +0700

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
let rec padLeft (s: string) (w: int) =
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
and indexOfFrom (s: string) (ch: string) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    let mutable start = start
    try
        let mutable i: int = start
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and containsStr (s: string) (sub: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable sub = sub
    try
        let mutable i: int = 0
        let sl: int = String.length s
        let subl: int = String.length sub
        while i <= (sl - subl) do
            if (s.Substring(i, (i + subl) - i)) = sub then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and distinct (slist: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable slist = slist
    try
        let mutable res: string array = [||]
        for s in slist do
            try
                let mutable found: bool = false
                for r in res do
                    try
                        if r = s then
                            found <- true
                            raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                if not found then
                    res <- Array.append res [|s|]
            with
            | Break -> ()
            | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and permutations (xs: string array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable xs = xs
    try
        if (int (Array.length xs)) <= 1 then
            __ret <- [|xs|]
            raise Return
        let mutable res: string array array = [||]
        let mutable i: int = 0
        while i < (int (Array.length xs)) do
            let mutable rest: string array = [||]
            let mutable j: int = 0
            while j < (int (Array.length xs)) do
                if j <> i then
                    rest <- Array.append rest [|xs.[j]|]
                j <- j + 1
            let subs: string array array = permutations rest
            for p in subs do
                let mutable perm: string array = [|xs.[i]|]
                let mutable k: int = 0
                while k < (Seq.length p) do
                    perm <- Array.append perm [|p.[k]|]
                    k <- k + 1
                res <- Array.append res [|perm|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and headTailOverlap (s1: string) (s2: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s1 = s1
    let mutable s2 = s2
    try
        let mutable start: int = 0
        while true do
            let ix: int = indexOfFrom s1 (s2.Substring(0, 1 - 0)) start
            if ix = (0 - 1) then
                __ret <- 0
                raise Return
            start <- ix
            if (s2.Substring(0, ((String.length s1) - start) - 0)) = (s1.Substring(start, (String.length s1) - start)) then
                __ret <- (String.length s1) - start
                raise Return
            start <- start + 1
        __ret
    with
        | Return -> __ret
and deduplicate (slist: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable slist = slist
    try
        let arr: string array = distinct slist
        let mutable filtered: string array = [||]
        let mutable i: int = 0
        try
            while i < (int (Array.length arr)) do
                let s1: string = arr.[i]
                let mutable within: bool = false
                let mutable j: int = 0
                try
                    while j < (int (Array.length arr)) do
                        if (j <> i) && (unbox<bool> (containsStr (unbox<string> (arr.[j])) s1)) then
                            within <- true
                            raise Break
                        j <- j + 1
                with
                | Break -> ()
                | Continue -> ()
                if not within then
                    filtered <- Array.append filtered [|s1|]
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- filtered
        raise Return
        __ret
    with
        | Return -> __ret
and joinAll (ss: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ss = ss
    try
        let mutable out: string = ""
        for s in ss do
            out <- out + (unbox<string> s)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and shortestCommonSuperstring (slist: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable slist = slist
    try
        let ss: string array = deduplicate slist
        let mutable shortest: string = joinAll ss
        let perms: string array array = permutations ss
        let mutable idx: int = 0
        while idx < (int (Array.length perms)) do
            let perm: string array = perms.[idx]
            let mutable sup: string = perm.[0]
            let mutable i: int = 0
            while i < (int ((int (Array.length ss)) - 1)) do
                let ov: int = headTailOverlap (unbox<string> (perm.[i])) (unbox<string> (perm.[i + 1]))
                sup <- sup + (perm.[i + 1].Substring(ov, (Seq.length (perm.[i + 1])) - ov))
                i <- i + 1
            if (String.length sup) < (String.length shortest) then
                shortest <- sup
            idx <- idx + 1
        __ret <- shortest
        raise Return
        __ret
    with
        | Return -> __ret
and printCounts (seq: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable seq = seq
    try
        let mutable a: int = 0
        let mutable c: int = 0
        let mutable g: int = 0
        let mutable t: int = 0
        let mutable i: int = 0
        while i < (String.length seq) do
            let ch: string = seq.Substring(i, (i + 1) - i)
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
            i <- i + 1
        let total: int = String.length seq
        printfn "%s" (("\nNucleotide counts for " + seq) + ":\n")
        printfn "%A" ((padLeft "A" 10) + (padLeft (string a) 12))
        printfn "%A" ((padLeft "C" 10) + (padLeft (string c) 12))
        printfn "%A" ((padLeft "G" 10) + (padLeft (string g) 12))
        printfn "%A" ((padLeft "T" 10) + (padLeft (string t) 12))
        printfn "%A" ((padLeft "Other" 10) + (padLeft (string (total - (((a + c) + g) + t))) 12))
        printfn "%s" "  ____________________"
        printfn "%A" ((padLeft "Total length" 14) + (padLeft (string total) 8))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tests: string array array = [|[|"TA"; "AAG"; "TA"; "GAA"; "TA"|]; [|"CATTAGGG"; "ATTAG"; "GGG"; "TA"|]; [|"AAGAUGGA"; "GGAGCGCAUC"; "AUCGCAAUAAGGA"|]; [|"ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT"; "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT"; "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA"; "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC"; "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT"; "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC"; "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT"; "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC"; "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC"; "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT"; "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC"; "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA"; "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"|]|]
        for seqs in tests do
            let scs: string = shortestCommonSuperstring (unbox<string array> seqs)
            printCounts scs
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
