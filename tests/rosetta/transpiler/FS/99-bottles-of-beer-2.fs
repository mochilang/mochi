// Generated 2025-07-24 15:22 +0700

exception Return

let rec fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable words: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if ((ch = " ") || (ch = "\n")) || (ch = "\t") then
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
and join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and numberName (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let small: string array = [|"no"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
        let tens: string array = [|"ones"; "ten"; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
        if n < 0 then
            __ret <- ""
            raise Return
        if n < 20 then
            __ret <- small.[n]
            raise Return
        if n < 100 then
            let mutable t = tens.[int (n / 10)]
            let mutable s: int = n % 10
            if s > 0 then
                t <- (t + " ") + (small.[s])
            __ret <- t
            raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and pluralizeFirst (s: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        if n = 1 then
            __ret <- s
            raise Return
        let w = fields s
        if (Seq.length w) > 0 then
            w.[0] <- (w.[0]) + "s"
        __ret <- join w " "
        raise Return
        __ret
    with
        | Return -> __ret
and randInt (seed: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable seed = seed
    let mutable n = n
    try
        let next: int = ((seed * 1664525) + 1013904223) % 2147483647
        __ret <- next % n
        raise Return
        __ret
    with
        | Return -> __ret
and slur (p: string) (d: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    let mutable d = d
    try
        if (String.length p) <= 2 then
            __ret <- p
            raise Return
        let mutable a: string array = [||]
        let mutable i: int = 1
        while i < ((String.length p) - 1) do
            a <- Array.append a [|p.Substring(i, (i + 1) - i)|]
            i <- i + 1
        let mutable idx = (Array.length a) - 1
        let mutable seed: int = d
        while idx >= 1 do
            seed <- ((seed * 1664525) + 1013904223) % 2147483647
            if (seed % 100) >= d then
                let j = seed % (idx + 1)
                let tmp = a.[idx]
                a.[idx] <- a.[j]
                a.[j] <- tmp
            idx <- idx - 1
        let mutable s: string = p.Substring(0, 1 - 0)
        let mutable k: int = 0
        while k < (Array.length a) do
            s <- s + (a.[k])
            k <- k + 1
        s <- s + (p.Substring((String.length p) - 1, (String.length p) - ((String.length p) - 1)))
        let w = fields s
        __ret <- join w " "
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable i: int = 99
        while i > 0 do
            printfn "%s" (((((slur (numberName i) i) + " ") + (pluralizeFirst (slur "bottle of" i) i)) + " ") + (slur "beer on the wall" i))
            printfn "%s" (((((slur (numberName i) i) + " ") + (pluralizeFirst (slur "bottle of" i) i)) + " ") + (slur "beer" i))
            printfn "%s" (((((slur "take one" i) + " ") + (slur "down" i)) + " ") + (slur "pass it around" i))
            printfn "%s" (((((slur (numberName (i - 1)) i) + " ") + (pluralizeFirst (slur "bottle of" i) (i - 1))) + " ") + (slur "beer on the wall" i))
            i <- i - 1
        __ret
    with
        | Return -> __ret
main()
