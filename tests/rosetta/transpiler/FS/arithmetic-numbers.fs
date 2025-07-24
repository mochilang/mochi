// Generated 2025-07-25 01:11 +0700

exception Return

let rec sieve (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    try
        let mutable spf: int array = [||]
        let mutable i: int = 0
        while i <= limit do
            spf <- Array.append spf [|0|]
            i <- i + 1
        i <- 2
        while i <= limit do
            if (spf.[i]) = 0 then
                spf.[i] <- i
                if (i * i) <= limit then
                    let mutable j: int = i * i
                    while j <= limit do
                        if (spf.[j]) = 0 then
                            spf.[j] <- i
                        j <- j + i
            i <- i + 1
        __ret <- spf
        raise Return
        __ret
    with
        | Return -> __ret
and primesFrom (spf: int array) (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable spf = spf
    let mutable limit = limit
    try
        let mutable primes: int array = [||]
        let mutable i: int = 3
        while i <= limit do
            if (spf.[i]) = i then
                primes <- Array.append primes [|i|]
            i <- i + 1
        __ret <- primes
        raise Return
        __ret
    with
        | Return -> __ret
and pad3 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        while (String.length s) < 3 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable out: string = ""
        let mutable i: int = (String.length s) - 1
        let mutable c: int = 0
        while i >= 0 do
            out <- (s.Substring(i, (i + 1) - i)) + out
            c <- c + 1
            if ((c % 3) = 0) && (i > 0) then
                out <- "," + out
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and primeCount (primes: int array) (last: int) (spf: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable primes = primes
    let mutable last = last
    let mutable spf = spf
    try
        let mutable lo: int = 0
        let mutable hi = Array.length primes
        while lo < hi do
            let mutable mid = int ((lo + hi) / 2)
            if (primes.[mid]) < last then
                lo <- mid + 1
            else
                hi <- mid
        let mutable count: int = lo + 1
        if (spf.[last]) <> last then
            count <- count - 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and arithmeticNumbers (limit: int) (spf: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    let mutable spf = spf
    try
        let mutable arr: int array = [|1|]
        let mutable n: int = 3
        while (Array.length arr) < limit do
            if (spf.[n]) = n then
                arr <- Array.append arr [|n|]
            else
                let mutable x: int = n
                let mutable sigma: int = 1
                let mutable tau: int = 1
                while x > 1 do
                    let mutable p = spf.[x]
                    if p = 0 then
                        p <- x
                    let mutable cnt: int = 0
                    let mutable power: int = p
                    let mutable sum: int = 1
                    while (x % p) = 0 do
                        x <- x / p
                        cnt <- cnt + 1
                        sum <- sum + power
                        power <- power * p
                    sigma <- sigma * sum
                    tau <- tau * (cnt + 1)
                if (sigma % tau) = 0 then
                    arr <- Array.append arr [|n|]
            n <- n + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let limit: int = 1228663
        let spf = sieve limit
        let primes = primesFrom spf limit
        let arr = arithmeticNumbers 1000000 spf
        printfn "%s" "The first 100 arithmetic numbers are:"
        let mutable i: int = 0
        while i < 100 do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < 10 do
                line <- line + (pad3 (arr.[i + j]))
                if j < 9 then
                    line <- line + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 10
        for x in [|1000; 10000; 100000; 1000000|] do
            let last = arr.[x - 1]
            let lastc = commatize last
            printfn "%s" ((("\nThe " + (commatize x)) + "th arithmetic number is: ") + lastc)
            let pc = primeCount primes last spf
            let comp = (x - pc) - 1
            printfn "%s" (((("The count of such numbers <= " + lastc) + " which are composite is ") + (commatize comp)) + ".")
        __ret
    with
        | Return -> __ret
main()
