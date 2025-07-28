// Generated 2025-07-28 07:48 +0700

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
let LIMIT: int = 999999
let primes: int array = primesUpTo LIMIT
let rec longestSeq (dir: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable dir = dir
    try
        let mutable pd: int = 0
        let mutable longSeqs: int array array = [|[|2|]|]
        let mutable currSeq: int array = [|2|]
        let mutable i: int = 1
        while i < (Seq.length primes) do
            let d: int = (primes.[i]) - (primes.[i - 1])
            if ((dir = "ascending") && (d <= pd)) || ((dir = "descending") && (d >= pd)) then
                if (Seq.length currSeq) > (Seq.length (longSeqs.[0])) then
                    longSeqs <- (match [|currSeq|] with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
                else
                    if (Seq.length currSeq) = (Seq.length (longSeqs.[0])) then
                        longSeqs <- Array.append longSeqs [|currSeq|]
                currSeq <- unbox<int array> [|primes.[i - 1]; primes.[i]|]
            else
                currSeq <- Array.append currSeq [|primes.[i]|]
            pd <- d
            i <- i + 1
        if (Seq.length currSeq) > (Seq.length (longSeqs.[0])) then
            longSeqs <- (match [|currSeq|] with | :? (int array array) as a -> a | :? (obj array) as oa -> oa |> Array.map (fun v -> unbox<int array> v) | _ -> failwith "invalid cast")
        else
            if (Seq.length currSeq) = (Seq.length (longSeqs.[0])) then
                longSeqs <- Array.append longSeqs [|currSeq|]
        printfn "%s" (((("Longest run(s) of primes with " + dir) + " differences is ") + (string (Seq.length (longSeqs.[0])))) + " :")
        for ls in longSeqs do
            let mutable diffs: int array = [||]
            let mutable j: int = 1
            while j < (Seq.length ls) do
                diffs <- Array.append diffs [|(ls.[j]) - (ls.[j - 1])|]
                j <- j + 1
            let mutable k: int = 0
            while k < ((Seq.length ls) - 1) do
                printfn "%s" (String.concat " " [|sprintf "%A" ((((string (ls.[k])) + " (") + (string (diffs.[k]))) + ") "); sprintf "%b" false|])
                k <- k + 1
            printfn "%s" (string (ls.[(Seq.length ls) - 1]))
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "For primes < 1 million:\n"
        for dir in [|"ascending"; "descending"|] do
            longestSeq (unbox<string> dir)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
