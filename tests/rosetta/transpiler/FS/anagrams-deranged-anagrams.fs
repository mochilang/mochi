// Generated 2025-07-26 05:05 +0700

exception Break
exception Continue

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
        let mutable n: int = Array.length arr
        let mutable m: int = 0
        while m < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (arr.[j]) > (arr.[j + 1]) then
                    let tmp: string = arr.[j]
                    arr.[j] <- arr.[j + 1]
                    arr.[j + 1] <- tmp
                j <- j + 1
            m <- m + 1
        let mutable out: string = ""
        i <- 0
        while i < n do
            out <- out + (unbox<string> (arr.[i]))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and deranged (a: string) (b: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (String.length a) <> (String.length b) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (String.length a) do
            if (a.Substring(i, (i + 1) - i)) = (b.Substring(i, (i + 1) - i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let words: string array = [|"constitutionalism"; "misconstitutional"|]
        let mutable m: Map<string, string array> = Map.ofList []
        let mutable bestLen: int = 0
        let mutable w1: string = ""
        let mutable w2: string = ""
        for w in words do
            try
                if (Seq.length w) <= bestLen then
                    raise Continue
                let k: string = sortRunes (unbox<string> w)
                if not (Map.containsKey k m) then
                    m <- Map.add k [|w|] m
                    raise Continue
                for c in m.[k] |> unbox<string array> do
                    try
                        if unbox<bool> (deranged (unbox<string> w) (unbox<string> c)) then
                            bestLen <- Seq.length w
                            w1 <- c
                            w2 <- w
                            raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                m <- Map.add k (Array.append (m.[k] |> unbox<string array>) [|w|]) m
            with
            | Break -> ()
            | Continue -> ()
        printfn "%s" ((((w1 + " ") + w2) + " : Length ") + (string bestLen))
        __ret
    with
        | Return -> __ret
main()
