// Generated 2025-07-26 05:05 +0700

exception Return

let rec countDivisors (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 2 then
            __ret <- 1
            raise Return
        let mutable count: int = 2
        let mutable i: int = 2
        while i <= (n / 2) do
            if (((n % i + i) % i)) = 0 then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "The first 20 anti-primes are:"
        let mutable maxDiv: int = 0
        let mutable count: int = 0
        let mutable n: int = 1
        let mutable line: string = ""
        while count < 20 do
            let d: int = countDivisors n
            if d > maxDiv then
                line <- (line + (string n)) + " "
                maxDiv <- d
                count <- count + 1
            n <- n + 1
        line <- line.Substring(0, ((String.length line) - 1) - 0)
        printfn "%s" line
        __ret
    with
        | Return -> __ret
main()
