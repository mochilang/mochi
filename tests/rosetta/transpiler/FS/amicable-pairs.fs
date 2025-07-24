// Generated 2025-07-24 20:52 +0700

exception Return

let rec pfacSum (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        let mutable sum: int = 0
        let mutable p: int = 1
        while p <= (i / 2) do
            if (i % p) = 0 then
                sum <- sum + p
            p <- p + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable sums: int array = [||]
        let mutable i: int = 0
        while i < 20000 do
            sums <- Array.append sums [|0|]
            i <- i + 1
        i <- 1
        while i < 20000 do
            sums.[i] <- pfacSum i
            i <- i + 1
        printfn "%s" "The amicable pairs below 20,000 are:"
        let mutable n: int = 2
        while n < 19999 do
            let m = sums.[n]
            if ((m > n) && (m < 20000)) && (n = (sums.[m])) then
                printfn "%s" ((("  " + (pad n 5)) + " and ") + (pad m 5))
            n <- n + 1
        __ret
    with
        | Return -> __ret
main()
