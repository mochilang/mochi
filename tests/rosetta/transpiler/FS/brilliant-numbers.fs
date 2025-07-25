// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let rec primesUpTo (n: int) =
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
            if unbox<bool> (sieve.[p]) then
                let mutable m: int = p * p
                while m <= n do
                    sieve.[m] <- false
                    m <- m + p
            p <- p + 1
        let mutable res: int array = [||]
        let mutable x: int = 2
        while x <= n do
            if unbox<bool> (sieve.[x]) then
                res <- Array.append res [|x|]
            x <- x + 1
        __ret <- res
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
        while (int (Array.length tmp)) > 0 do
            let mutable min = tmp.[0]
            let mutable idx: int = 0
            let mutable i: int = 1
            while i < (int (Array.length tmp)) do
                if (tmp.[i]) < min then
                    min <- tmp.[i]
                    idx <- i
                i <- i + 1
            res <- Array.append res [|min|]
            let mutable out: int array = [||]
            let mutable j: int = 0
            while j < (int (Array.length tmp)) do
                if j <> idx then
                    out <- Array.append out [|tmp.[j]|]
                j <- j + 1
            tmp <- out
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable i: int = (String.length s) - 3
        while i >= 1 do
            s <- ((s.Substring(0, i - 0)) + ",") + (s.Substring(i, (String.length s) - i))
            i <- i - 3
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable primes: int array = primesUpTo 3200000
let rec getBrilliant (digits: int) (limit: int) (countOnly: bool) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable digits = digits
    let mutable limit = limit
    let mutable countOnly = countOnly
    try
        let mutable brilliant: int array = [||]
        let mutable count: int = 0
        let mutable pow: int = 1
        let mutable next: int = 999999999999999L
        let mutable k: int = 1
        try
            while k <= digits do
                let mutable s: int array = [||]
                for p in primes do
                    try
                        if (int p) >= (pow * 10) then
                            raise Break
                        if (int p) > pow then
                            s <- Array.append s [|p|]
                    with
                    | Break -> ()
                    | Continue -> ()
                let mutable i: int = 0
                try
                    while i < (int (Array.length s)) do
                        let mutable j: int = i
                        try
                            while j < (int (Array.length s)) do
                                let mutable prod = (s.[i]) * (s.[j])
                                if (int prod) < limit then
                                    if countOnly then
                                        count <- count + 1
                                    else
                                        brilliant <- Array.append brilliant [|prod|]
                                else
                                    if prod < next then
                                        next <- prod
                                    raise Break
                                j <- j + 1
                        with
                        | Break -> ()
                        | Continue -> ()
                        i <- i + 1
                with
                | Break -> ()
                | Continue -> ()
                pow <- pow * 10
                k <- k + 1
        with
        | Break -> ()
        | Continue -> ()
        if countOnly then
            __ret <- Map.ofList [("bc", box count); ("next", box next)]
            raise Return
        __ret <- Map.ofList [("bc", box brilliant); ("next", box next)]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "First 100 brilliant numbers:"
        let r: Map<string, obj> = getBrilliant 2 10000 false
        let mutable br: int array = sortInts (unbox<int array> (r.["bc"]))
        br <- Array.sub br 0 (100 - 0)
        let mutable i: int = 0
        while i < (int (Array.length br)) do
            printfn "%s" (String.concat " " [|sprintf "%A" ((unbox<string> (string (br.[i]).padStart(4, " "))) + " "); sprintf "%b" false|])
            if ((((i + 1) % 10 + 10) % 10)) = 0 then
                printfn "%s" (String.concat " " [|sprintf "%A" ""; sprintf "%b" true|])
            i <- i + 1
        printfn "%s" (String.concat " " [|sprintf "%A" ""; sprintf "%b" true|])
        let mutable k: int = 1
        while k <= 13 do
            let limit = pow 10 k
            let r2: Map<string, obj> = getBrilliant k (int limit) true
            let total: obj = r2.["bc"]
            let next: obj = r2.["next"]
            let climit: string = commatize (int limit)
            let ctotal: string = commatize (int ((unbox<int> total) + 1))
            let cnext: string = commatize (unbox<int> next)
            printfn "%s" ((((("First >= " + (unbox<string> (climit.padStart(18, " ")))) + " is ") + (unbox<string> (ctotal.padStart(14, " ")))) + " in the series: ") + (unbox<string> (cnext.padStart(18, " "))))
            k <- k + 1
        __ret
    with
        | Return -> __ret
