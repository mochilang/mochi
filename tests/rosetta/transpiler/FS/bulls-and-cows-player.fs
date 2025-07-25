// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

open System

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
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
and fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if ((ch = " ") || (ch = "\t")) || (ch = "\n") then
                if (String.length cur) > 0 then
                    words <- Array.append words [|cur|]
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            words <- Array.append words [|cur|]
        __ret <- words
        raise Return
        __ret
    with
        | Return -> __ret
and makePatterns () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        let digits: string array = [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]
        let mutable pats: string array = [||]
        let mutable i: int = 0
        while i < (int (Array.length digits)) do
            let mutable j: int = 0
            while j < (int (Array.length digits)) do
                if j <> i then
                    let mutable k: int = 0
                    while k < (int (Array.length digits)) do
                        if (k <> i) && (k <> j) then
                            let mutable l: int = 0
                            while l < (int (Array.length digits)) do
                                if ((l <> i) && (l <> j)) && (l <> k) then
                                    pats <- Array.append pats [|(((digits.[i]) + (digits.[j])) + (digits.[k])) + (digits.[l])|]
                                l <- l + 1
                        k <- k + 1
                j <- j + 1
            i <- i + 1
        __ret <- pats
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" ((((("Cows and bulls/player\n" + "You think of four digit number of unique digits in the range 1 to 9.\n") + "I guess.  You score my guess:\n") + "    A correct digit but not in the correct place is a cow.\n") + "    A correct digit in the correct place is a bull.\n") + "You give my score as two numbers separated with a space.")
        let mutable patterns: string array = makePatterns()
        try
            while true do
                if (int (Array.length patterns)) = 0 then
                    printfn "%s" "Oops, check scoring."
                    __ret <- ()
                    raise Return
                let guess: string = patterns.[0]
                patterns <- Array.sub patterns 1 ((int (Array.length patterns)) - 1)
                let mutable cows: int = 0
                let mutable bulls: int = 0
                try
                    while true do
                        printfn "%s" (("My guess: " + guess) + ".  Score? (c b) ")
                        let line: string = System.Console.ReadLine()
                        let toks: string array = fields line
                        if (int (Array.length toks)) = 2 then
                            let c: int = int (toks.[0])
                            let b: int = int (toks.[1])
                            if ((((c >= 0) && (c <= 4)) && (b >= 0)) && (b <= 4)) && ((c + b) <= 4) then
                                cows <- c
                                bulls <- b
                                raise Break
                        printfn "%s" "Score guess as two numbers: cows bulls"
                with
                | Break -> ()
                | Continue -> ()
                if bulls = 4 then
                    printfn "%s" "I did it. :)"
                    __ret <- ()
                    raise Return
                let mutable next: string array = [||]
                let mutable idx: int = 0
                while idx < (int (Array.length patterns)) do
                    let pat: string = patterns.[idx]
                    let mutable c: int = 0
                    let mutable b: int = 0
                    let mutable i: int = 0
                    while i < 4 do
                        let cg: string = guess.Substring(i, (i + 1) - i)
                        let cp: string = pat.Substring(i, (i + 1) - i)
                        if cg = cp then
                            b <- b + 1
                        else
                            if (int (indexOf pat cg)) >= 0 then
                                c <- c + 1
                        i <- i + 1
                    if (c = cows) && (b = bulls) then
                        next <- Array.append next [|pat|]
                    idx <- idx + 1
                patterns <- next
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
main()
