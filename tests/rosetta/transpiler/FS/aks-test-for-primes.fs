// Generated 2025-07-24 20:52 +0700

exception Return

let rec poly (p: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        let mutable s: string = ""
        let mutable coef: int = 1
        let mutable i: int = p
        if coef <> 1 then
            s <- s + (string coef)
        while i > 0 do
            s <- s + "x"
            if i <> 1 then
                s <- (s + "^") + (string i)
            coef <- int ((coef * i) / ((p - i) + 1))
            let mutable d: int = coef
            if ((p - (i - 1)) % 2) = 1 then
                d <- -d
            if d < 0 then
                s <- (s + " - ") + (string (-d))
            else
                s <- (s + " + ") + (string d)
            i <- i - 1
        if s = "" then
            s <- "1"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and aks (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        let mutable c: int = n
        let mutable i: int = 1
        while i < n do
            if (c % n) <> 0 then
                __ret <- false
                raise Return
            c <- int ((c * (n - i)) / (i + 1))
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable p: int = 0
        while p <= 7 do
            printfn "%s" (((string p) + ":  ") + (poly p))
            p <- p + 1
        let mutable first: bool = true
        p <- 2
        let mutable line: string = ""
        while p < 50 do
            if aks p then
                if first then
                    line <- line + (string p)
                    first <- false
                else
                    line <- (line + " ") + (string p)
            p <- p + 1
        printfn "%s" line
        __ret
    with
        | Return -> __ret
main()
