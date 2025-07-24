// Generated 2025-07-25 00:53 +0700

exception Return

let rec primeFactors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable factors: int array = [||]
        let mutable x: int = n
        while (x % 2) = 0 do
            factors <- Array.append factors [|2|]
            x <- int (x / 2)
        let mutable p: int = 3
        while (p * p) <= x do
            while (x % p) = 0 do
                factors <- Array.append factors [|p|]
                x <- int (x / p)
            p <- p + 2
        if x > 1 then
            factors <- Array.append factors [|x|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + ch
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and D (n: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        if n < 0.0 then
            __ret <- -(D (-n))
            raise Return
        if n < 2.0 then
            __ret <- 0.0
            raise Return
        let mutable factors: int array = [||]
        if n < 10000000000000000000.0 then
            factors <- primeFactors (int n)
        else
            let g = int (n / 100.0)
            factors <- primeFactors g
            factors <- Array.append factors [|2|]
            factors <- Array.append factors [|2|]
            factors <- Array.append factors [|5|]
            factors <- Array.append factors [|5|]
        let c = Array.length factors
        if c = 1 then
            __ret <- 1.0
            raise Return
        if c = 2 then
            __ret <- float ((factors.[0]) + (factors.[1]))
            raise Return
        let d = n / (float (factors.[0]))
        __ret <- ((D d) * (float (factors.[0]))) + d
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        while (String.length s) < 4 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable vals: int array = [||]
        let mutable n: int = -99
        while n < 101 do
            vals <- Array.append vals [|int (D (float n))|]
            n <- n + 1
        let mutable i: int = 0
        while i < (Array.length vals) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < 10 do
                line <- line + (pad (vals.[i + j]))
                if j < 9 then
                    line <- line + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 10
        let mutable pow: float = 1.0
        let mutable m: int = 1
        while m < 21 do
            pow <- pow * 10.0
            let mutable exp: string = string m
            if (String.length exp) < 2 then
                exp <- exp + " "
            let mutable res: string = (string m) + (repeat "0" (m - 1))
            printfn "%s" ((("D(10^" + exp) + ") / 7 = ") + res)
            m <- m + 1
        __ret
    with
        | Return -> __ret
main()
