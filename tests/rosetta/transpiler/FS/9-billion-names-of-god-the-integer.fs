// Generated 2025-07-24 15:11 +0700

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
            let mutable s = (av + bv) + carry
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
and minInt (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a < b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
and cumu (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable cache: int array array array = [|[|bigFromInt 1|]|]
        let mutable y: int = 1
        while y <= n do
            let mutable row: int array array = [|bigFromInt 0|]
            let mutable x: int = 1
            while x <= y do
                let ``val`` = cache.[y - x].[minInt x (y - x)]
                row <- Array.append row [|bigAdd (row.[(Array.length row) - 1]) ``val``|]
                x <- x + 1
            cache <- Array.append cache [|row|]
            y <- y + 1
        __ret <- cache.[n]
        raise Return
        __ret
    with
        | Return -> __ret
and row (n: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable n = n
    try
        let e = cumu n
        let mutable out: string array = [||]
        let mutable i: int = 0
        while i < n do
            let diff = bigSub (e.[i + 1]) (e.[i])
            out <- Array.append out [|bigToString diff|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" "rows:"
let mutable x: int = 1
while x < 11 do
    let r = row x
    let mutable line: string = ""
    let mutable i: int = 0
    while i < (Seq.length r) do
        line <- ((line + " ") + (r.[i])) + " "
        i <- i + 1
    printfn "%s" line
    x <- x + 1
printfn "%s" ""
printfn "%s" "sums:"
for num in [|23; 123; 1234|] do
    let r = cumu num
    printfn "%s" (((string num) + " ") + (bigToString (r.[(Seq.length r) - 1])))
