// Generated 2025-07-25 01:11 +0700

exception Return

let rec intSqrt (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if x < 2 then
            __ret <- x
            raise Return
        let mutable left: int = 1
        let mutable right: int = x / 2
        let mutable ans: int = 0
        while left <= right do
            let mid: int = left + ((right - left) / 2)
            let sq: int = mid * mid
            if sq = x then
                __ret <- mid
                raise Return
            if sq < x then
                left <- mid + 1
                ans <- mid
            else
                right <- mid - 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and sumRecip (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable s: int = 1
        let limit = intSqrt n
        let mutable f: int = 2
        while f <= limit do
            if (n % f) = 0 then
                s <- s + (n / f)
                let f2: int = n / f
                if f2 <> f then
                    s <- s + f
            f <- f + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let nums: int array = [|6; 28; 120; 496; 672; 8128; 30240; 32760; 523776|]
        for n in nums do
            let s = sumRecip n
            if (s % n) = 0 then
                let ``val`` = s / n
                let mutable perfect: string = ""
                if ``val`` = 1 then
                    perfect <- "perfect!"
                printfn "%s" ((((("Sum of recipr. factors of " + (string n)) + " = ") + (string ``val``)) + " exactly ") + perfect)
        __ret
    with
        | Return -> __ret
main()
