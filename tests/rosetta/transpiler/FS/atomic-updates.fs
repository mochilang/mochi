// Generated 2025-07-25 17:29 +0000

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
let rec randOrder (seed: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable seed = seed
    let mutable n = n
    try
        let next: int = ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
and randChaos (seed: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable seed = seed
    let mutable n = n
    try
        let next: int = ((((seed * 1103515245) + 12345) % 2147483647 + 2147483647) % 2147483647)
        __ret <- [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nBuckets: int = 10
        let initialSum: int = 1000
        let mutable buckets: int array = [||]
        for i in 0 .. (nBuckets - 1) do
            buckets <- Array.append buckets [|0|]
        let mutable i: int = nBuckets
        let mutable dist: int = initialSum
        while i > 0 do
            let v: int = dist / i
            i <- i - 1
            buckets.[i] <- v
            dist <- dist - v
        let mutable tc0: int = 0
        let mutable tc1: int = 0
        let mutable total: int = 0
        let mutable nTicks: int = 0
        let mutable seedOrder: int = 1
        let mutable seedChaos: int = 2
        printfn "%s" "sum  ---updates---    mean  buckets"
        let mutable t: int = 0
        while t < 5 do
            let mutable r: int array = randOrder seedOrder nBuckets
            seedOrder <- r.[0]
            let mutable b1: int = r.[1]
            let mutable b2: int = (((b1 + 1) % nBuckets + nBuckets) % nBuckets)
            let v1: int = buckets.[b1]
            let v2: int = buckets.[b2]
            if v1 > v2 then
                let mutable a: int = int ((v1 - v2) / 2)
                if a > (int (buckets.[b1])) then
                    a <- buckets.[b1]
                buckets.[b1] <- (int (buckets.[b1])) - a
                buckets.[b2] <- (int (buckets.[b2])) + a
            else
                let mutable a: int = int ((v2 - v1) / 2)
                if a > (int (buckets.[b2])) then
                    a <- buckets.[b2]
                buckets.[b2] <- (int (buckets.[b2])) - a
                buckets.[b1] <- (int (buckets.[b1])) + a
            tc0 <- tc0 + 1
            r <- randChaos seedChaos nBuckets
            seedChaos <- r.[0]
            b1 <- r.[1]
            b2 <- (((b1 + 1) % nBuckets + nBuckets) % nBuckets)
            r <- randChaos seedChaos (int ((int (buckets.[b1])) + 1))
            seedChaos <- r.[0]
            let mutable amt: int = r.[1]
            if amt > (int (buckets.[b1])) then
                amt <- buckets.[b1]
            buckets.[b1] <- (int (buckets.[b1])) - amt
            buckets.[b2] <- (int (buckets.[b2])) + amt
            tc1 <- tc1 + 1
            let mutable sum: int = 0
            let mutable idx: int = 0
            while idx < nBuckets do
                sum <- sum + (int (buckets.[idx]))
                idx <- idx + 1
            total <- (total + tc0) + tc1
            nTicks <- nTicks + 1
            printfn "%s" (((((((((string sum) + " ") + (string tc0)) + " ") + (string tc1)) + " ") + (string (total / nTicks))) + "  ") + (string buckets))
            tc0 <- 0
            tc1 <- 0
            t <- t + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
