// Generated 2025-07-27 15:57 +0700

exception Break
exception Continue

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let rec primesUpTo (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable sieve: bool array = [||]
        let mutable i: int = 0
        while i <= n do
            sieve <- unbox<bool array> (Array.append sieve [|true|])
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
                res <- unbox<int array> (Array.append res [|x|])
            x <- x + 1
        __ret <- unbox<int array> res
        raise Return
        __ret
    with
        | Return -> __ret
and sortInts (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable tmp: int array = xs
        while (unbox<int> (Array.length tmp)) > 0 do
            let mutable min: int = tmp.[0]
            let mutable idx: int = 0
            let mutable i: int = 1
            while i < (unbox<int> (Array.length tmp)) do
                if (unbox<int> (tmp.[i])) < min then
                    min <- unbox<int> (tmp.[i])
                    idx <- i
                i <- i + 1
            res <- unbox<int array> (Array.append res [|min|])
            let mutable out: int array = [||]
            let mutable j: int = 0
            while j < (unbox<int> (Array.length tmp)) do
                if j <> idx then
                    out <- unbox<int array> (Array.append out [|tmp.[j]|])
                j <- j + 1
            tmp <- unbox<int array> out
        __ret <- unbox<int array> res
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
                        if (unbox<int> p) >= (pow * 10) then
                            raise Break
                        if (unbox<int> p) > pow then
                            s <- unbox<int array> (Array.append s [|p|])
                    with
                    | Break -> ()
                    | Continue -> ()
                let mutable i: int = 0
                try
                    while i < (unbox<int> (Array.length s)) do
                        let mutable j: int = i
                        try
                            while j < (unbox<int> (Array.length s)) do
                                let mutable prod = (s.[i]) * (s.[j])
                                if (unbox<int> prod) < limit then
                                    if countOnly then
                                        count <- count + 1
                                    else
                                        brilliant <- unbox<int array> (Array.append brilliant [|prod|])
                                else
                                    if prod < next then
                                        next <- unbox<int> prod
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
            __ret <- unbox<Map<string, obj>> (Map.ofList [("bc", box count); ("next", box next)])
            raise Return
        __ret <- unbox<Map<string, obj>> (Map.ofList [("bc", box brilliant); ("next", box next)])
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "First 100 brilliant numbers:"
        let r: Map<string, obj> = getBrilliant 2 10000 false
        let mutable br: int array = sortInts (unbox<int array> (r.["bc"]))
        br <- unbox<int array> (Array.sub br 0 (100 - 0))
        let mutable i: int = 0
        while i < (unbox<int> (Array.length br)) do
            printfn "%s" (String.concat " " [|sprintf "%A" ((unbox<string> (string (br.[i]).padStart(4, " "))) + " "); sprintf "%b" false|])
            if ((((i + 1) % 10 + 10) % 10)) = 0 then
                printfn "%s" (String.concat " " [|sprintf "%A" ""; sprintf "%b" true|])
            i <- i + 1
        printfn "%s" (String.concat " " [|sprintf "%A" ""; sprintf "%b" true|])
        let mutable k: int = 1
        while k <= 13 do
            let limit = pow 10 k
            let r2: Map<string, obj> = getBrilliant k (unbox<int> limit) true
            let total: obj = box (r2.["bc"])
            let next: obj = box (r2.["next"])
            let climit: string = commatize (unbox<int> limit)
            let ctotal: string = commatize (unbox<int> ((unbox<int> total) + 1))
            let cnext: string = commatize (unbox<int> next)
            printfn "%s" ((((("First >= " + (unbox<string> (climit.padStart(18, " ")))) + " is ") + (unbox<string> (ctotal.padStart(14, " ")))) + " in the series: ") + (unbox<string> (cnext.padStart(18, " "))))
            k <- k + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
