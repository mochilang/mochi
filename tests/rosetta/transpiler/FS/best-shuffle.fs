// Generated 2025-07-26 04:38 +0700

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
let rec nextRand (seed: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable seed = seed
    try
        __ret <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        raise Return
        __ret
    with
        | Return -> __ret
and shuffleChars (s: string) (seed: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable s = s
    let mutable seed = seed
    try
        let mutable chars: string array = [||]
        let mutable i: int = 0
        while i < (String.length s) do
            chars <- Array.append chars [|s.Substring(i, (i + 1) - i)|]
            i <- i + 1
        let mutable sd: int = seed
        let mutable idx: int = (int (Array.length chars)) - 1
        while idx > 0 do
            sd <- nextRand sd
            let mutable j: int = ((sd % (idx + 1) + (idx + 1)) % (idx + 1))
            let tmp: string = chars.[idx]
            chars.[idx] <- chars.[j]
            chars.[j] <- tmp
            idx <- idx - 1
        let mutable res: string = ""
        i <- 0
        while i < (int (Array.length chars)) do
            res <- res + (unbox<string> (chars.[i]))
            i <- i + 1
        __ret <- [|box res; box sd|]
        raise Return
        __ret
    with
        | Return -> __ret
and bestShuffle (s: string) (seed: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable s = s
    let mutable seed = seed
    try
        let r: obj array = shuffleChars s seed
        let mutable t: obj = r.[0]
        let mutable sd: obj = r.[1]
        let mutable arr: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length t) do
            arr <- Array.append arr [|t.Substring(i, (i + 1) - i)|]
            i <- i + 1
        i <- 0
        try
            while i < (int (Array.length arr)) do
                let mutable j: int = 0
                try
                    while j < (int (Array.length arr)) do
                        if ((i <> j) && ((unbox<string> (arr.[i])) <> (s.Substring(j, (j + 1) - j)))) && ((unbox<string> (arr.[j])) <> (s.Substring(i, (i + 1) - i))) then
                            let tmp: string = arr.[i]
                            arr.[i] <- arr.[j]
                            arr.[j] <- tmp
                            raise Break
                        j <- j + 1
                with
                | Break -> ()
                | Continue -> ()
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        let mutable count: int = 0
        i <- 0
        while i < (int (Array.length arr)) do
            if (unbox<string> (arr.[i])) = (s.Substring(i, (i + 1) - i)) then
                count <- count + 1
            i <- i + 1
        let mutable out: string = ""
        i <- 0
        while i < (int (Array.length arr)) do
            out <- out + (unbox<string> (arr.[i]))
            i <- i + 1
        __ret <- [|box out; sd; box count|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ts: string array = [|"abracadabra"; "seesaw"; "elk"; "grrrrrr"; "up"; "a"|]
        let mutable seed: int = 1
        let mutable i: int = 0
        while i < (int (Array.length ts)) do
            let r: obj array = bestShuffle (unbox<string> (ts.[i])) seed
            let shuf: obj = r.[0]
            seed <- r.[1]
            let cnt: obj = r.[2]
            printfn "%s" ((((((unbox<string> (ts.[i])) + " -> ") + (unbox<string> shuf)) + " (") + (string cnt)) + ")")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
