// Generated 2025-08-04 20:44 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (((String.length (sep)) > 0) && ((i + (String.length (sep))) <= (String.length (s)))) && ((_substring s i (i + (String.length (sep)))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length (sep))
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length (str)) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length (str)) do
            n <- (n * 10) + (digits.[(string (str.Substring(i, (i + 1) - i)))])
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and joinInts (nums: int array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable nums = nums
    let mutable sep = sep
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            if i > 0 then
                s <- s + sep
            s <- s + (string (_idx nums (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and undot (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable parts: string array = split (s) (".")
        let mutable nums: int array = [||]
        for p in parts do
            nums <- Array.append nums [|parseIntStr (p)|]
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
and factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable f: int = 1
        let mutable i: int = 2
        while i <= n do
            f <- f * i
            i <- i + 1
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and genFactBaseNums (size: int) (countOnly: bool) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable size = size
    let mutable countOnly = countOnly
    try
        let mutable results: int array array = [||]
        let mutable count: int = 0
        let mutable n: int = 0
        try
            while true do
                try
                    let mutable radix: int = 2
                    let mutable res: int array = [||]
                    if not countOnly then
                        let mutable z: int = 0
                        while z < size do
                            res <- Array.append res [|0|]
                            z <- z + 1
                    let mutable k: int = n
                    while k > 0 do
                        let mutable div: int = k / radix
                        let mutable rem: int = ((k % radix + radix) % radix)
                        if (not countOnly) && (radix <= (size + 1)) then
                            res.[(size - radix) + 1] <- rem
                        k <- div
                        radix <- radix + 1
                    if radix > (size + 2) then
                        raise Break
                    count <- count + 1
                    if not countOnly then
                        results <- Array.append results [|res|]
                    n <- n + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- [|box (results); box (count)|]
        raise Return
        __ret
    with
        | Return -> __ret
and mapToPerms (factNums: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable factNums = factNums
    try
        let mutable perms: int array array = [||]
        let mutable psize: int = (Seq.length (_idx factNums (0))) + 1
        let mutable start: int array = [||]
        let mutable i: int = 0
        while i < psize do
            start <- Array.append start [|i|]
            i <- i + 1
        for fn in factNums do
            let mutable perm: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (start)) do
                perm <- Array.append perm [|_idx start (j)|]
                j <- j + 1
            let mutable m: int = 0
            while m < (Seq.length (fn)) do
                let mutable g: int = _idx fn (m)
                if g <> 0 then
                    let mutable first: int = m
                    let mutable last: int = m + g
                    let mutable t: int = 1
                    while t <= g do
                        let mutable temp: int = _idx perm (first)
                        let mutable x: int = first + 1
                        while x <= last do
                            perm.[x - 1] <- _idx perm (x)
                            x <- x + 1
                        perm.[last] <- temp
                        t <- t + 1
                m <- m + 1
            perms <- Array.append perms [|perm|]
        __ret <- perms
        raise Return
        __ret
    with
        | Return -> __ret
let mutable seed: int = 1
let rec randInt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- ((seed % n + n) % n)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let g: obj array = genFactBaseNums (3) (false)
        let mutable factNums: obj = _idx g (0)
        let mutable perms: int array array = mapToPerms ((match factNums with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast"))
        let mutable i: int = 0
        while i < (int ((unbox<System.Array> factNums).Length)) do
            printfn "%s" (((joinInts (unbox<int array> (((factNums :?> System.Array).GetValue(i)))) (".")) + " -> ") + (joinInts (_idx perms (i)) ("")))
            i <- i + 1
        let count2: int = factorial (11)
        printfn "%s" ("\nPermutations generated = " + (string (count2)))
        printfn "%s" ("compared to 11! which  = " + (string (factorial (11))))
        printfn "%s" ("")
        let fbn51s: string array = [|"39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0"; "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"|]
        factNums <- [|undot (_idx fbn51s (0)); undot (_idx fbn51s (1))|]
        perms <- mapToPerms ((match factNums with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast"))
        let shoe: string = "A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣"
        let mutable cards: string array = [||]
        i <- 0
        while i < 52 do
            let mutable card: string = _substring shoe (2 * i) ((2 * i) + 2)
            if (card.Substring(0, 1 - 0)) = "T" then
                card <- "10" + (card.Substring(1, 2 - 1))
            cards <- Array.append cards [|card|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (fbn51s)) do
            printfn "%s" (_idx fbn51s (i))
            let mutable perm: int array = _idx perms (i)
            let mutable j: int = 0
            let mutable line: string = ""
            while j < (Seq.length (perm)) do
                line <- line + (_idx cards (_idx perm (j)))
                j <- j + 1
            printfn "%s" (line + "\n")
            i <- i + 1
        let mutable fbn51: int array = [||]
        i <- 0
        while i < 51 do
            fbn51 <- Array.append fbn51 [|randInt (52 - i)|]
            i <- i + 1
        printfn "%s" (joinInts (fbn51) ("."))
        perms <- mapToPerms ([|fbn51|])
        let mutable line: string = ""
        i <- 0
        while i < (Seq.length (_idx perms (0))) do
            line <- line + (_idx cards (_idx (_idx perms (0)) (i)))
            i <- i + 1
        printfn "%s" (line)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
