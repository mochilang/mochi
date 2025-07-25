// Generated 2025-07-25 13:04 +0700

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
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable d: int = 0
        let mutable a: int = 0
        let mutable pnum: int = 0
        let mutable i: int = 1
        while i <= 20000 do
            let j = pfacSum i
            if j < i then
                d <- d + 1
            if j = i then
                pnum <- pnum + 1
            if j > i then
                a <- a + 1
            i <- i + 1
        printfn "%s" (("There are " + (string d)) + " deficient numbers between 1 and 20000")
        printfn "%s" (("There are " + (string a)) + " abundant numbers  between 1 and 20000")
        printfn "%s" (("There are " + (string pnum)) + " perfect numbers between 1 and 20000")
        __ret
    with
        | Return -> __ret
main()
