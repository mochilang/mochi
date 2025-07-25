// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and firstPrimeFactor (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 1 then
            __ret <- 1
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- 3
            raise Return
        if (((n % 5 + 5) % 5)) = 0 then
            __ret <- 5
            raise Return
        let mutable inc: int array = [|4; 2; 4; 2; 4; 6; 2; 6|]
        let mutable k: int = 7
        let mutable i: int = 0
        while (k * k) <= n do
            if (((n % k + k) % k)) = 0 then
                __ret <- k
                raise Return
            k <- k + (int (inc.[i]))
            i <- (((i + 1) % (Array.length inc) + (Array.length inc)) % (Array.length inc))
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (n: int) (width: int) =
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
and formatFloat (f: float) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    let mutable prec = prec
    try
        let s: string = string f
        let idx: int = indexOf s "."
        if idx < 0 then
            __ret <- s
            raise Return
        let need: int = (idx + 1) + prec
        if (String.length s) > need then
            __ret <- s.Substring(0, need - 0)
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable blum: int array = [||]
        let mutable counts: int array = [|0; 0; 0; 0|]
        let mutable digits: int array = [|1; 3; 7; 9|]
        let mutable i: int = 1
        let mutable bc: int = 0
        try
            while true do
                let p: int = firstPrimeFactor i
                if (((p % 4 + 4) % 4)) = 3 then
                    let q: int = int (i / p)
                    if ((q <> p) && ((((q % 4 + 4) % 4)) = 3)) && (unbox<bool> (isPrime q)) then
                        if bc < 50 then
                            blum <- Array.append blum [|i|]
                        let d: int = ((i % 10 + 10) % 10)
                        if d = 1 then
                            counts.[0] <- (int (counts.[0])) + 1
                        else
                            if d = 3 then
                                counts.[1] <- (int (counts.[1])) + 1
                            else
                                if d = 7 then
                                    counts.[2] <- (int (counts.[2])) + 1
                                else
                                    if d = 9 then
                                        counts.[3] <- (int (counts.[3])) + 1
                        bc <- bc + 1
                        if bc = 50 then
                            printfn "%s" "First 50 Blum integers:"
                            let mutable idx: int = 0
                            while idx < 50 do
                                let mutable line: string = ""
                                let mutable j: int = 0
                                while j < 10 do
                                    line <- (line + (unbox<string> (padLeft (int (blum.[idx])) 3))) + " "
                                    idx <- idx + 1
                                    j <- j + 1
                                printfn "%s" (line.Substring(0, ((String.length line) - 1) - 0))
                            raise Break
                if (((i % 5 + 5) % 5)) = 3 then
                    i <- i + 4
                else
                    i <- i + 2
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
main()
