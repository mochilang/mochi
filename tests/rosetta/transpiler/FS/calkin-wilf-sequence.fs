// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let rec bigrat (a: int) (b: int) =
    let mutable __ret : bignum = Unchecked.defaultof<bignum>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (unbox<bignum> a) / (unbox<bignum> b)
        raise Return
        __ret
    with
        | Return -> __ret
and calkinWilf (n: int) =
    let mutable __ret : bignum array = Unchecked.defaultof<bignum array>
    let mutable n = n
    try
        let mutable seq: bignum array = [||]
        seq <- Array.append seq [|bigrat 1 1|]
        let mutable i: int = 1
        while i < n do
            let mutable prev: bignum = seq.[i - 1]
            let a: bigint = num prev
            let b: bigint = denom prev
            let f: bigint = a / b
            let mutable t: bignum = bigrat f 1
            t <- t * (unbox<bignum> 2)
            t <- t - prev
            t <- t + (unbox<bignum> 1)
            t <- (unbox<bignum> 1) / t
            seq <- Array.append seq [|t|]
            i <- i + 1
        __ret <- seq
        raise Return
        __ret
    with
        | Return -> __ret
and toContinued (r: bignum) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r = r
    try
        let mutable a: bigint = num r
        let mutable b: bigint = denom r
        let mutable res: int array = [||]
        try
            while true do
                res <- Array.append res [|int (a / b)|]
                let t: bigint = ((a % b + b) % b)
                a <- b
                b <- t
                if a = (bigint 1) then
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        if (int ((((Array.length res) % 2 + 2) % 2))) = 0 then
            res.[(int (Array.length res)) - 1] <- (int (res.[(int (Array.length res)) - 1])) - 1
            res <- Array.append res [|1|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and termNumber (cf: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable cf = cf
    try
        let mutable b: string = ""
        let mutable d: string = "1"
        for n in cf do
            b <- (unbox<string> (repeat d (int n))) + b
            if d = "1" then
                d <- "0"
            else
                d <- "1"
        __ret <- parseIntStr b 2
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
        let mutable i: int = 0
        let mutable cnt: int = 0
        let mutable neg: bool = false
        if (s.Substring(0, 1 - 0)) = "-" then
            neg <- true
            s <- s.Substring(1, (String.length s) - 1)
        i <- (String.length s) - 1
        while i >= 0 do
            out <- (s.Substring(i, (i + 1) - i)) + out
            cnt <- cnt + 1
            if (cnt = 3) && (i <> 0) then
                out <- "," + out
                cnt <- 0
            i <- i - 1
        if neg then
            out <- "-" + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let cw: bignum array = calkinWilf 20
        printfn "%s" "The first 20 terms of the Calkin-Wilf sequnence are:"
        let mutable i: int = 0
        while i < 20 do
            let r: bignum = cw.[i]
            let s: string = string (num r)
            if (int (denom r)) <> 1 then
                s <- (s + "/") + (string (denom r))
            printfn "%s" (((unbox<string> (i + (int 1).padStart(2, " "))) + ": ") + s)
            i <- i + 1
        let r: bignum = bigrat 83116 51639
        let cf: int array = toContinued r
        let tn: int = termNumber cf
        printfn "%s" (((((("" + (string (num r))) + "/") + (string (denom r))) + " is the ") + (unbox<string> (commatize tn))) + "th term of the sequence.")
        __ret
    with
        | Return -> __ret
main()
