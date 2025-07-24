// Generated 2025-07-24 20:52 +0700

exception Break
exception Continue

exception Return

let rec divisors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable divs: int array = [|1|]
        let mutable divs2: int array = [||]
        let mutable i: int = 2
        while (i * i) <= n do
            if (n % i) = 0 then
                let j = int (n / i)
                divs <- Array.append divs [|i|]
                if i <> j then
                    divs2 <- Array.append divs2 [|j|]
            i <- i + 1
        let mutable j = (Array.length divs2) - 1
        while j >= 0 do
            divs <- Array.append divs [|divs2.[j]|]
            j <- j - 1
        __ret <- divs
        raise Return
        __ret
    with
        | Return -> __ret
and sum (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable tot: int = 0
        for v in xs do
            tot <- tot + v
        __ret <- tot
        raise Return
        __ret
    with
        | Return -> __ret
and sumStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- (s + (string (xs.[i]))) + " + "
            i <- i + 1
        __ret <- s.Substring(0, ((String.length s) - 3) - 0)
        raise Return
        __ret
    with
        | Return -> __ret
and pad2 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let s: string = string n
        if (String.length s) < 2 then
            __ret <- " " + s
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad5 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        while (String.length s) < 5 do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and abundantOdd (searchFrom: int) (countFrom: int) (countTo: int) (printOne: bool) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable searchFrom = searchFrom
    let mutable countFrom = countFrom
    let mutable countTo = countTo
    let mutable printOne = printOne
    try
        let mutable count: int = countFrom
        let mutable n: int = searchFrom
        try
            while count < countTo do
                let divs = divisors n
                let tot: int = Seq.sum divs
                if tot > n then
                    count <- count + 1
                    if printOne && (count < countTo) then
                        n <- n + 2
                        raise Continue
                    let s = sumStr divs
                    if not printOne then
                        printfn "%s" (((((((pad2 count) + ". ") + (pad5 n)) + " < ") + s) + " = ") + (string tot))
                    else
                        printfn "%s" (((((string n) + " < ") + s) + " = ") + (string tot))
                n <- n + 2
        with
        | Break -> ()
        | Continue -> ()
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let max: int = 25
        printfn "%s" (("The first " + (string max)) + " abundant odd numbers are:")
        let n = abundantOdd 1 0 max false
        printfn "%s" "\nThe one thousandth abundant odd number is:"
        abundantOdd n max 1000 true
        printfn "%s" "\nThe first abundant odd number above one billion is:"
        abundantOdd 1000000001 0 1 true
        __ret
    with
        | Return -> __ret
main()
