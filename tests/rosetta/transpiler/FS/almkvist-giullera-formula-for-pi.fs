// Generated 2025-07-24 20:52 +0700

exception Break
exception Continue

exception Return

let rec bigTrim (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable n: int = Seq.length a
        while (n > 1) && ((a.[n - 1]) = 0) do
            a <- Array.sub a 0 ((n - 1) - 0)
            n <- n - 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and bigFromInt (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable x = x
    try
        if x = 0 then
            __ret <- [|0|]
            raise Return
        let mutable digits: int array = [||]
        let mutable n: int = x
        while n > 0 do
            digits <- Array.append digits [|n % 10|]
            n <- n / 10
        __ret <- digits
        raise Return
        __ret
    with
        | Return -> __ret
and bigCmp (a: int array) (b: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length a) > (Seq.length b) then
            __ret <- 1
            raise Return
        if (Seq.length a) < (Seq.length b) then
            __ret <- -1
            raise Return
        let mutable i: int = (Seq.length a) - 1
        while i >= 0 do
            if (a.[i]) > (b.[i]) then
                __ret <- 1
                raise Return
            if (a.[i]) < (b.[i]) then
                __ret <- -1
                raise Return
            i <- i - 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and bigAdd (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = [||]
        let mutable carry: int = 0
        let mutable i: int = 0
        while ((i < (Seq.length a)) || (i < (Seq.length b))) || (carry > 0) do
            let mutable av: int = 0
            if i < (Seq.length a) then
                av <- a.[i]
            let mutable bv: int = 0
            if i < (Seq.length b) then
                bv <- b.[i]
            let mutable s: int = (av + bv) + carry
            res <- Array.append res [|s % 10|]
            carry <- s / 10
            i <- i + 1
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
and bigSub (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = [||]
        let mutable borrow: int = 0
        let mutable i: int = 0
        while i < (Seq.length a) do
            let mutable av = a.[i]
            let mutable bv: int = 0
            if i < (Seq.length b) then
                bv <- b.[i]
            let mutable diff = (av - bv) - borrow
            if diff < 0 then
                diff <- diff + 10
                borrow <- 1
            else
                borrow <- 0
            res <- Array.append res [|diff|]
            i <- i + 1
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
and bigMulSmall (a: int array) (m: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable m = m
    try
        if m = 0 then
            __ret <- [|0|]
            raise Return
        let mutable res: int array = [||]
        let mutable carry: int = 0
        let mutable i: int = 0
        while i < (Seq.length a) do
            let mutable prod = ((a.[i]) * m) + carry
            res <- Array.append res [|prod % 10|]
            carry <- prod / 10
            i <- i + 1
        while carry > 0 do
            res <- Array.append res [|carry % 10|]
            carry <- carry / 10
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
and bigMulBig (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < ((Seq.length a) + (Seq.length b)) do
            res <- Array.append res [|0|]
            i <- i + 1
        i <- 0
        while i < (Seq.length a) do
            let mutable carry: int = 0
            let mutable j: int = 0
            while j < (Seq.length b) do
                let mutable idx: int = i + j
                let mutable prod = ((res.[idx]) + ((a.[i]) * (b.[j]))) + carry
                res.[idx] <- prod % 10
                carry <- prod / 10
                j <- j + 1
            let mutable idx: int = i + (Seq.length b)
            while carry > 0 do
                let mutable prod = (res.[idx]) + carry
                res.[idx] <- prod % 10
                carry <- prod / 10
                idx <- idx + 1
            i <- i + 1
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
and bigMulPow10 (a: int array) (k: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable k = k
    try
        let mutable i: int = 0
        while i < k do
            a <- Array.append [|0|] a
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and bigDivSmall (a: int array) (m: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable m = m
    try
        let mutable res: int array = [||]
        let mutable rem: int = 0
        let mutable i: int = (Seq.length a) - 1
        while i >= 0 do
            let mutable cur = (rem * 10) + (a.[i])
            let mutable q = cur / m
            rem <- cur % m
            res <- Array.append [|q|] res
            i <- i - 1
        __ret <- bigTrim res
        raise Return
        __ret
    with
        | Return -> __ret
and bigToString (a: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    try
        let mutable s: string = ""
        let mutable i: int = (Seq.length a) - 1
        while i >= 0 do
            s <- s + (string (a.[i]))
            i <- i - 1
        __ret <- s
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
and sortInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable tmp = xs
        while (Seq.length tmp) > 0 do
            let mutable min = tmp.[0]
            let mutable idx: int = 0
            let mutable i: int = 1
            while i < (Seq.length tmp) do
                if (tmp.[i]) < min then
                    min <- tmp.[i]
                    idx <- i
                i <- i + 1
            res <- Array.append res [|min|]
            let mutable out: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length tmp) do
                if j <> idx then
                    out <- Array.append out [|tmp.[j]|]
                j <- j + 1
            tmp <- out
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and primesUpTo (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable sieve: bool array = [||]
        let mutable i: int = 0
        while i <= n do
            sieve <- Array.append sieve [|true|]
            i <- i + 1
        let mutable p: int = 2
        while (p * p) <= n do
            if sieve.[p] then
                let mutable m: int = p * p
                while m <= n do
                    sieve.[m] <- false
                    m <- m + p
            p <- p + 1
        let mutable res: int array = [||]
        let mutable x: int = 2
        while x <= n do
            if sieve.[x] then
                res <- Array.append res [|x|]
            x <- x + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and factorialExp (n: int) (primes: int array) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable n = n
    let mutable primes = primes
    try
        let mutable m: Map<string, int> = Map.ofList []
        for p in primes do
            try
                if p > n then
                    raise Break
                let mutable t: int = n
                let mutable e: int = 0
                while t > 0 do
                    t <- t / p
                    e <- e + t
                m <- Map.add (string p) e m
            with
            | Break -> ()
            | Continue -> ()
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and factorSmall (x: int) (primes: int array) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable x = x
    let mutable primes = primes
    try
        let mutable f: Map<string, int> = Map.ofList []
        let mutable n: int = x
        for p in primes do
            try
                if (p * p) > n then
                    raise Break
                let mutable c: int = 0
                while (n % p) = 0 do
                    c <- c + 1
                    n <- n / p
                if c > 0 then
                    f <- Map.add (string p) c f
            with
            | Break -> ()
            | Continue -> ()
        if n > 1 then
            f <- Map.add (string n) ((f.get(string n, 0)) + 1) f
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and computeIP (n: int) (primes: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable primes = primes
    try
        let mutable exps = factorialExp (6 * n) primes
        let fn = factorialExp n primes
        for k in fn do
            exps.[k] <- (exps.get(k, 0)) - (6 * (fn.[k]))
        exps.["2"] <- (exps.get("2", 0)) + 5
        let t2: int = (((532 * n) * n) + (126 * n)) + 9
        let ft2 = factorSmall t2 primes
        for k in ft2 do
            exps.[k] <- (exps.get(k, 0)) + (ft2.[k])
        exps.["3"] <- (exps.get("3", 0)) - 1
        let mutable keys: int array = [||]
        for k in exps do
            keys <- Array.append keys [|int k|]
        keys <- sortInts keys
        let mutable res = bigFromInt 1
        for p in keys do
            let mutable e = exps.[string p]
            let mutable i: int = 0
            while i < e do
                res <- bigMulSmall res p
                i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and formatTerm (ip: int array) (pw: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ip = ip
    let mutable pw = pw
    try
        let mutable s = bigToString ip
        if pw >= (Seq.length s) then
            let mutable frac = (repeat "0" (pw - (Seq.length s))) + s
            if (Seq.length frac) < 33 then
                frac <- frac + (repeat "0" (33 - (Seq.length frac)))
            __ret <- "0." + (frac.Substring(0, 33 - 0))
            raise Return
        let mutable intpart: string = s.Substring(0, ((Seq.length s) - pw) - 0)
        let mutable frac: string = s.Substring((Seq.length s) - pw, (Seq.length s) - ((Seq.length s) - pw))
        if (String.length frac) < 33 then
            frac <- frac + (repeat "0" (33 - (String.length frac)))
        __ret <- (intpart + ".") + (frac.Substring(0, 33 - 0))
        raise Return
        __ret
    with
        | Return -> __ret
and bigAbsDiff (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if (bigCmp a b) >= 0 then (bigSub a b) else (bigSub b a)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let primes = primesUpTo 2000
        printfn "%s" "N                               Integer Portion  Pow  Nth Term (33 dp)"
        let line = repeat "-" 89
        printfn "%A" line
        let mutable sum = bigFromInt 0
        let mutable prev = bigFromInt 0
        let mutable denomPow: int = 0
        let mutable n: int = 0
        try
            while true do
                let ip = computeIP n primes
                let pw: int = (6 * n) + 3
                if pw > denomPow then
                    sum <- bigMulPow10 sum (pw - denomPow)
                    prev <- bigMulPow10 prev (pw - denomPow)
                    denomPow <- pw
                if n < 10 then
                    let termStr = formatTerm ip pw
                    let mutable ipStr = bigToString ip
                    while (Seq.length ipStr) < 44 do
                        ipStr <- " " + ipStr
                    let mutable pwStr: string = string (-pw)
                    while (String.length pwStr) < 3 do
                        pwStr <- " " + pwStr
                    let mutable padTerm = termStr
                    while (Seq.length padTerm) < 35 do
                        padTerm <- padTerm + " "
                    printfn "%s" (((((((string n) + "  ") + ipStr) + "  ") + pwStr) + "  ") + padTerm)
                sum <- bigAdd sum ip
                let diff = bigAbsDiff sum prev
                if (denomPow >= 70) && ((bigCmp diff (bigMulPow10 (bigFromInt 1) (denomPow - 70))) < 0) then
                    raise Break
                prev <- sum
                n <- n + 1
        with
        | Break -> ()
        | Continue -> ()
        let precision: int = 70
        let target = bigMulPow10 (bigFromInt 1) (denomPow + (2 * precision))
        let mutable low = bigFromInt 0
        let mutable high = bigMulPow10 (bigFromInt 1) (precision + 1)
        while (bigCmp low (bigSub high (bigFromInt 1))) < 0 do
            let mutable mid = bigDivSmall (bigAdd low high) 2
            let mutable prod = bigMulBig (bigMulBig mid mid) sum
            if (bigCmp prod target) <= 0 then
                low <- mid
            else
                high <- bigSub mid (bigFromInt 1)
        let mutable piInt = low
        let mutable piStr = bigToString piInt
        if (Seq.length piStr) <= precision then
            piStr <- (repeat "0" ((precision - (Seq.length piStr)) + 1)) + piStr
        let mutable out: string = ((piStr.Substring(0, ((Seq.length piStr) - precision) - 0)) + ".") + (piStr.Substring((Seq.length piStr) - precision, (Seq.length piStr) - ((Seq.length piStr) - precision)))
        printfn "%s" ""
        printfn "%s" "Pi to 70 decimal places is:"
        printfn "%s" out
        __ret
    with
        | Return -> __ret
main()
