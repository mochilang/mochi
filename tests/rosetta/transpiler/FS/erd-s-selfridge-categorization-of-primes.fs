// Generated 2025-08-03 15:40 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
open System.Collections.Generic

let rec generatePrimes (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable primes: int array = [|2|]
        let mutable cand: int = 3
        try
            while (Seq.length (primes)) < n do
                try
                    let mutable isP: bool = true
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (primes)) do
                            try
                                let p: int = primes.[i]
                                if (p * p) > cand then
                                    raise Break
                                if (((cand % p + p) % p)) = 0 then
                                    isP <- false
                                    raise Break
                                i <- i + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if isP then
                        primes <- Array.append primes [|cand|]
                    cand <- cand + 2
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- primes
        raise Return
        __ret
    with
        | Return -> __ret
and primeFactors (n: int) (primes: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable primes = primes
    try
        let mutable factors: int array = [||]
        let mutable num: int = n
        let mutable i: int = 0
        while (i < (Seq.length (primes))) && (((primes.[i]) * (primes.[i])) <= num) do
            let p: int = primes.[i]
            while (((num % p + p) % p)) = 0 do
                factors <- Array.append factors [|p|]
                num <- num / p
            i <- i + 1
        if num > 1 then
            factors <- Array.append factors [|num|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
let mutable prevCats: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
let rec cat (p: int) (primes: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    let mutable primes = primes
    try
        if prevCats.ContainsKey(p) then
            __ret <- unbox<int> (prevCats.[p])
            raise Return
        let mutable pf: int array = primeFactors (p + 1) (primes)
        let mutable all23: bool = true
        try
            for f in pf do
                try
                    if (f <> 2) && (f <> 3) then
                        all23 <- false
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if all23 then
            prevCats.[p] <- 1
            __ret <- 1
            raise Return
        if p > 2 then
            let mutable unique: int array = [||]
            let mutable last: int = -1
            for f in pf do
                if f <> last then
                    unique <- Array.append unique [|f|]
                    last <- f
            pf <- unique
        let mutable c: int = 2
        try
            while c <= 11 do
                try
                    let mutable ok: bool = true
                    try
                        for f in pf do
                            try
                                if (cat (f) (primes)) >= c then
                                    ok <- false
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if ok then
                        prevCats.[p] <- c
                        __ret <- c
                        raise Return
                    c <- c + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        prevCats.[p] <- 12
        __ret <- 12
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string (n)
        while (String.length (s)) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable primes: int array = generatePrimes (1000)
        let mutable es: int array array = [||]
        for _ in 0 .. (12 - 1) do
            es <- Array.append es [|[||]|]
        printfn "%s" ("First 200 primes:\n")
        let mutable idx: int = 0
        while idx < 200 do
            let p: int = primes.[idx]
            let mutable c: int = cat (p) (primes)
            es.[c - 1] <- Array.append (es.[c - 1]) [|p|]
            idx <- idx + 1
        let mutable c: int = 1
        while c <= 6 do
            if (Seq.length (es.[c - 1])) > 0 then
                printfn "%s" (("Category " + (string (c))) + ":")
                printfn "%s" (("[" + (unbox<string> (String.concat (" ") (Array.toList (Array.map string (es.[c - 1])))))) + "]")
                printfn "%s" ("")
            c <- c + 1
        printfn "%s" ("First thousand primes:\n")
        while idx < 1000 do
            let p: int = primes.[idx]
            let cv: int = cat (p) (primes)
            es.[cv - 1] <- Array.append (es.[cv - 1]) [|p|]
            idx <- idx + 1
        c <- 1
        while c <= 12 do
            let e: int array = es.[c - 1]
            if (Seq.length (e)) > 0 then
                let line: string = (((((("Category " + (padLeft (c) (2))) + ": First = ") + (padLeft (e.[0]) (7))) + "  Last = ") + (padLeft (e.[(Seq.length (e)) - 1]) (8))) + "  Count = ") + (padLeft (Seq.length (e)) (6))
                printfn "%s" (line)
            c <- c + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
