// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let rec indexOfStr (h: string) (n: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable n = n
    try
        let hlen: int = String.length h
        let nlen: int = String.length n
        if nlen = 0 then
            __ret <- 0
            raise Return
        let mutable i: int = 0
        while i <= (hlen - nlen) do
            if (h.Substring(i, (i + nlen) - i)) = n then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and stringSearchSingle (h: string) (n: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable n = n
    try
        __ret <- indexOfStr h n
        raise Return
        __ret
    with
        | Return -> __ret
and stringSearch (h: string) (n: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable h = h
    let mutable n = n
    try
        let mutable result: int array = [||]
        let mutable start: int = 0
        let hlen: int = String.length h
        let nlen: int = String.length n
        try
            while start < hlen do
                let idx: int = indexOfStr (h.Substring(start, hlen - start)) n
                if idx >= 0 then
                    result <- Array.append result [|start + idx|]
                    start <- (start + idx) + nlen
                else
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and display (nums: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable nums = nums
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (int (Array.length nums)) do
            if i > 0 then
                s <- s + ", "
            s <- s + (string (nums.[i]))
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
        let texts: string array = [|"GCTAGCTCTACGAGTCTA"; "GGCTATAATGCGTA"; "there would have been a time for such a word"; "needle need noodle needle"; "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages"; "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."|]
        let patterns: string array = [|"TCTA"; "TAATAAA"; "word"; "needle"; "and"; "alfalfa"|]
        let mutable i: int = 0
        while i < (int (Array.length texts)) do
            printfn "%s" ((("text" + (string (i + 1))) + " = ") + (unbox<string> (texts.[i])))
            i <- i + 1
        printfn "%s" ""
        let mutable j: int = 0
        while j < (int (Array.length texts)) do
            let idxs: int array = stringSearch (unbox<string> (texts.[j])) (unbox<string> (patterns.[j]))
            printfn "%s" ((((("Found \"" + (unbox<string> (patterns.[j]))) + "\" in 'text") + (string (j + 1))) + "' at indexes ") + (unbox<string> (display idxs)))
            j <- j + 1
        __ret
    with
        | Return -> __ret
main()
