// Generated 2025-07-24 20:52 +0700

exception Return

let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (n % 2) = 0 then
            __ret <- n = 2
            raise Return
        if (n % 3) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (n % d) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (n % d) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and sumDigits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable s: int = 0
        let mutable x: int = n
        while x > 0 do
            s <- s + (x % 10)
            x <- int (x / 10)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n < 10 then
            __ret <- "  " + (string n)
            raise Return
        if n < 100 then
            __ret <- " " + (string n)
            raise Return
        __ret <- string n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        printfn "%s" "Additive primes less than 500:"
        let mutable count: int = 0
        let mutable line: string = ""
        let mutable lineCount: int = 0
        let mutable i: int = 2
        while i < 500 do
            if (isPrime i) && (isPrime (sumDigits i)) then
                count <- count + 1
                line <- (line + (pad i)) + "  "
                lineCount <- lineCount + 1
                if lineCount = 10 then
                    printfn "%s" (line.Substring(0, ((String.length line) - 2) - 0))
                    line <- ""
                    lineCount <- 0
            if i > 2 then
                i <- i + 2
            else
                i <- i + 1
        if lineCount > 0 then
            printfn "%s" (line.Substring(0, ((String.length line) - 2) - 0))
        printfn "%s" ((string count) + " additive primes found.")
        __ret
    with
        | Return -> __ret
main()
